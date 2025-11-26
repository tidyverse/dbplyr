test_that("dplyr.strict_sql = TRUE prevents auto conversion", {
  withr::local_options(dplyr.strict_sql = TRUE)
  local_con(simulate_dbi())

  expect_equal(test_translate_sql(1 + 2), sql("1.0 + 2.0"))
  expect_snapshot(error = TRUE, {
    test_translate_sql(blah(x))
    test_translate_sql(x %blah% y)
  })
})

test_that("namespace calls are translated", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(dplyr::n(), window = FALSE), sql("COUNT(*)"))
  expect_equal(test_translate_sql(base::ceiling(x)), sql("CEIL(`x`)"))

  expect_snapshot(error = TRUE, {
    test_translate_sql(NOSUCHPACKAGE::foo())
    test_translate_sql(dbplyr::NOSUCHFUNCTION())
    test_translate_sql(base::abbreviate(x))
  })

  lz <- lazy_frame(x = 1)
  # Also test full pipeline to ensure that they make it through partial_eval
  expect_snapshot(error = TRUE, {
    lz %>% mutate(x = NOSUCHPACKAGE::foo())
    lz %>% mutate(x = dbplyr::NOSUCHFUNCTION())
    lz %>% mutate(x = base::abbreviate(x))
  })
})

test_that("Wrong number of arguments raises error", {
  local_con(simulate_dbi())
  expect_error(
    test_translate_sql(mean(1, 2, na.rm = TRUE), window = FALSE),
    "unused argument"
  )
})

test_that("between translated to special form (#503)", {
  local_con(simulate_dbi())
  out <- test_translate_sql(between(x, 1, 2))
  expect_equal(out, sql("`x` BETWEEN 1.0 AND 2.0"))
})

test_that("%in% translation parenthesises when needed", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(x %in% 1L), sql("`x` IN (1)"))
  expect_equal(test_translate_sql(x %in% c(1L)), sql("`x` IN (1)"))
  expect_equal(test_translate_sql(x %in% 1:2), sql("`x` IN (1, 2)"))
  expect_equal(test_translate_sql(x %in% y), sql("`x` IN `y`"))
})

test_that("%in% strips vector names", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(x %in% c(a = 1L)), sql("`x` IN (1)"))
  expect_equal(test_translate_sql(x %in% !!c(a = 1L)), sql("`x` IN (1)"))
})

test_that("%in% with empty vector", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(x %in% !!integer()), sql("FALSE"))
})

test_that("n_distinct(x) translated to COUNT(distinct, x)", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(n_distinct(x), window = FALSE),
    sql("COUNT(DISTINCT `x`)")
  )
  expect_equal(
    test_translate_sql(n_distinct(x), window = TRUE),
    sql("COUNT(DISTINCT `x`) OVER ()")
  )
  expect_error(
    test_translate_sql(n_distinct(x, y), window = FALSE),
    "unused argument"
  )
})

test_that("na_if is translated to NULLIF (#211)", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(na_if(x, 0L)), sql("NULLIF(`x`, 0)"))
})

test_that("connection affects quoting character", {
  lf <- lazy_frame(field1 = 1, field2 = 2, con = simulate_sqlite())
  out <- select(lf, field1)
  expect_match(sql_render(out), "^SELECT `field1`\nFROM `df`$")
})

test_that("magrittr pipe is translated", {
  local_con(simulate_dbi())
  expect_identical(
    test_translate_sql(1 %>% is.na()),
    test_translate_sql(is.na(1))
  )
})

test_that("user infix functions are translated", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(x %like% y), sql("`x` like `y`"))

  # keep case and also works with vectors of length > 1 #1299
  expect_equal(test_translate_sql(x %LIKE% 1:2), sql("`x` LIKE (1, 2)"))
})

test_that("sql() evaluates input locally", {
  local_con(simulate_dbi())
  a <- "x"
  expect_equal(test_translate_sql(a), sql("`a`"))
  expect_equal(test_translate_sql(sql(a)), sql("x"))

  f <- function() {
    a <- "y"
    test_translate_sql(sql(paste0(a)))
  }
  expect_equal(f(), sql("y"))
})

test_that("sql() evaluates input locally in across()", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(
    lf %>%
      summarise(
        across(x, ~ sql(gsub("x", "y", cur_column())))
      ) %>%
      remote_query(),
    sql("SELECT y AS `x`\nFROM `df`")
  )
})

# casts -------------------------------------------------------------------

test_that("casts as expected", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(as.integer64(x)), sql("CAST(`x` AS BIGINT)"))
  expect_equal(test_translate_sql(as.logical(x)), sql("CAST(`x` AS BOOLEAN)"))
  expect_equal(test_translate_sql(as.Date(x)), sql("CAST(`x` AS DATE)"))
})

# numeric -----------------------------------------------------------------

test_that("hypergeometric functions use manual calculation", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(cosh(x)), sql("(EXP(`x`) + EXP(-(`x`))) / 2"))
  expect_equal(test_translate_sql(sinh(x)), sql("(EXP(`x`) - EXP(-(`x`))) / 2"))
  expect_equal(
    test_translate_sql(tanh(x)),
    sql("(EXP(2 * (`x`)) - 1) / (EXP(2 * (`x`)) + 1)")
  )
  expect_equal(
    test_translate_sql(coth(x)),
    sql("(EXP(2 * (`x`)) + 1) / (EXP(2 * (`x`)) - 1)")
  )
})

test_that("pmin and max use GREATEST and LEAST", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(pmin(x, y, z, na.rm = TRUE)),
    sql("LEAST(`x`, `y`, `z`)")
  )
  expect_equal(
    test_translate_sql(pmax(x, y, na.rm = TRUE)),
    sql("GREATEST(`x`, `y`)")
  )
})

test_that("round uses integer digits", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(round(10.1)), sql("ROUND(10.1, 0)"))
  expect_equal(
    test_translate_sql(round(10.1, digits = 1)),
    sql("ROUND(10.1, 1)")
  )
})

# string functions --------------------------------------------------------

test_that("paste() translated to CONCAT_WS", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(paste0(x, y)), sql("CONCAT_WS('', `x`, `y`)"))
  expect_equal(test_translate_sql(paste(x, y)), sql("CONCAT_WS(' ', `x`, `y`)"))
  expect_equal(
    test_translate_sql(paste(x, y, sep = ",")),
    sql("CONCAT_WS(',', `x`, `y`)")
  )
})

# stringr -------------------------------------------

test_that("str_length() translates correctly ", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(str_length(x)), sql("LENGTH(`x`)"))
})

test_that("lower/upper translates correctly ", {
  local_con(simulate_dbi())
  expect_equal(test_translate_sql(str_to_upper(x)), sql("UPPER(`x`)"))
  expect_equal(test_translate_sql(str_to_lower(x)), sql("LOWER(`x`)"))
})

test_that("str_trim() translates correctly ", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(str_trim(x, "both")),
    sql("LTRIM(RTRIM(`x`))")
  )
  expect_equal(test_translate_sql(str_trim(x, "left")), sql("LTRIM(`x`)"))
  expect_equal(test_translate_sql(str_trim(x, "right")), sql("RTRIM(`x`)"))
})

# subsetting --------------------------------------------------------------

test_that("[ treated as if it is logical subsetting", {
  local_con(simulate_dbi())
  expect_equal(
    test_translate_sql(y[x == 0L]),
    sql("CASE WHEN (`x` = 0) THEN (`y`) END")
  )
})
