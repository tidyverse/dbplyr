test_that("dplyr.strict_sql = TRUE prevents auto conversion", {
  withr::local_options(dplyr.strict_sql = TRUE)
  con <- simulate_dbi()

  expect_translation(con, 1 + 2, "1.0 + 2.0")
  expect_snapshot(error = TRUE, {
    translate_sql(blah(x), con = con)
    translate_sql(x %blah% y, con = con)
  })
})

test_that("namespace calls are translated", {
  con <- simulate_dbi()
  expect_translation(con, dplyr::n(), "COUNT(*)", window = FALSE)
  expect_translation(con, base::ceiling(x), "CEIL(`x`)")

  expect_snapshot(error = TRUE, {
    translate_sql(NOSUCHPACKAGE::foo(), con = con)
    translate_sql(dbplyr::NOSUCHFUNCTION(), con = con)
    translate_sql(base::abbreviate(x), con = con)
  })

  lz <- lazy_frame(x = 1)
  # Also test full pipeline to ensure that they make it through partial_eval
  expect_snapshot(error = TRUE, {
    lz |> mutate(x = NOSUCHPACKAGE::foo())
    lz |> mutate(x = dbplyr::NOSUCHFUNCTION())
    lz |> mutate(x = base::abbreviate(x))
  })
})

test_that("Wrong number of arguments raises error", {
  con <- simulate_dbi()
  expect_error(
    translate_sql(mean(1, 2, na.rm = TRUE), con = con, window = FALSE),
    "unused argument"
  )
})

test_that("between translated to special form (#503)", {
  con <- simulate_dbi()
  expect_translation(con, between(x, 1, 2), "`x` BETWEEN 1.0 AND 2.0")
})

test_that("%in% translation parenthesises when needed", {
  con <- simulate_dbi()
  expect_translation(con, x %in% 1L, "`x` IN (1)")
  expect_translation(con, x %in% c(1L), "`x` IN (1)")
  expect_translation(con, x %in% 1:2, "`x` IN (1, 2)")
  expect_translation(con, x %in% y, "`x` IN `y`")
})

test_that("%in% strips vector names", {
  con <- simulate_dbi()
  expect_translation(con, x %in% c(a = 1L), "`x` IN (1)")
  expect_translation(con, x %in% !!c(a = 1L), "`x` IN (1)")
})

test_that("%in% with empty vector", {
  con <- simulate_dbi()
  expect_translation(con, x %in% !!integer(), "FALSE")
})

test_that("n_distinct(x) translated to COUNT(distinct, x)", {
  con <- simulate_dbi()
  expect_translation(con, n_distinct(x), "COUNT(DISTINCT `x`)", window = FALSE)
  expect_translation(
    con,
    n_distinct(x),
    "COUNT(DISTINCT `x`) OVER ()",
    window = TRUE
  )
})

test_that("na_if is translated to NULLIF (#211)", {
  con <- simulate_dbi()
  expect_translation(con, na_if(x, 0L), "NULLIF(`x`, 0)")
})

test_that("connection affects quoting character", {
  lf <- lazy_frame(field1 = 1, field2 = 2, con = simulate_sqlite())
  out <- select(lf, field1)
  expect_match(sql_render(out), "^SELECT `field1`\nFROM `df`$")
})

test_that("magrittr pipe is translated", {
  con <- simulate_dbi()
  expect_identical(
    translate_sql(1 |> is.na(), con = con),
    translate_sql(is.na(1), con = con)
  )
})

test_that("user infix functions are translated", {
  con <- simulate_dbi()
  expect_translation(con, x %foo% y, "`x` foo `y`")

  # keep case and also works with vectors of length > 1 #1299
  expect_translation(con, x %foo% (1:2), "`x` foo (1, 2)")
})

test_that("sql() evaluates input locally", {
  con <- simulate_dbi()
  a <- "x"
  expect_translation(con, a, "`a`")
  expect_translation(con, sql(a), "x")

  f <- function() {
    a <- "y"
    translate_sql(sql(paste0(a)), con = con)
  }

  expect_equal(f(), sql("y"))
})

test_that("sql() evaluates input locally in across()", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(
    lf |>
      summarise(
        across(x, ~ sql(gsub("x", "y", cur_column())))
      ) |>
      remote_query(),
    sql("SELECT y AS `x`\nFROM `df`")
  )
})

# casts -------------------------------------------------------------------

test_that("casts as expected", {
  con <- simulate_dbi()
  expect_translation(con, as.integer64(x), "CAST(`x` AS BIGINT)")
  expect_translation(con, as.logical(x), "CAST(`x` AS BOOLEAN)")
  expect_translation(con, as.Date(x), "CAST(`x` AS DATE)")
})

# numeric -----------------------------------------------------------------

test_that("hypergeometric functions use manual calculation", {
  con <- simulate_dbi()
  expect_translation(con, cosh(x), "(EXP(`x`) + EXP(-(`x`))) / 2")
  expect_translation(con, sinh(x), "(EXP(`x`) - EXP(-(`x`))) / 2")
  expect_translation(
    con,
    tanh(x),
    "(EXP(2 * (`x`)) - 1) / (EXP(2 * (`x`)) + 1)"
  )
  expect_translation(
    con,
    coth(x),
    "(EXP(2 * (`x`)) + 1) / (EXP(2 * (`x`)) - 1)"
  )
})

test_that("pmin and max use GREATEST and LEAST", {
  con <- simulate_dbi()
  expect_translation(con, pmin(x, y, z, na.rm = TRUE), "LEAST(`x`, `y`, `z`)")
  expect_translation(con, pmax(x, y, na.rm = TRUE), "GREATEST(`x`, `y`)")
})

test_that("round uses integer digits", {
  con <- simulate_dbi()
  expect_translation(con, round(10.1), "ROUND(10.1, 0)")
  expect_translation(con, round(10.1, digits = 1), "ROUND(10.1, 1)")
})

# string functions --------------------------------------------------------

test_that("paste() translated to CONCAT_WS", {
  con <- simulate_dbi()
  expect_translation(con, paste0(x, y), "CONCAT_WS('', `x`, `y`)")
  expect_translation(con, paste(x, y), "CONCAT_WS(' ', `x`, `y`)")
  expect_translation(con, paste(x, y, sep = ","), "CONCAT_WS(',', `x`, `y`)")
})

# stringr -------------------------------------------

test_that("str_length() translates correctly ", {
  con <- simulate_dbi()
  expect_translation(con, str_length(x), "LENGTH(`x`)")
})

test_that("lower/upper translates correctly ", {
  con <- simulate_dbi()
  expect_translation(con, str_to_upper(x), "UPPER(`x`)")
  expect_translation(con, str_to_lower(x), "LOWER(`x`)")
})

test_that("str_trim() translates correctly ", {
  con <- simulate_dbi()
  expect_translation(con, str_trim(x, "both"), "LTRIM(RTRIM(`x`))")
  expect_translation(con, str_trim(x, "left"), "LTRIM(`x`)")
  expect_translation(con, str_trim(x, "right"), "RTRIM(`x`)")
})

# subsetting --------------------------------------------------------------

test_that("[ treated as if it is logical subsetting", {
  con <- simulate_dbi()
  expect_translation(con, y[x == 0L], "CASE WHEN (`x` = 0) THEN (`y`) END")
})
