test_that("uses FETCH FIRST instead of LIMIT", {
  mf <- lazy_frame(x = 1, y = 2, con = dialect_db2())
  expect_snapshot(mf |> head())
  expect_snapshot(mf |> head(10))
})

test_that("uses double quotes for identifiers", {
  con <- dialect_db2()
  expect_translation(con, first_name, '"first_name"')
})

test_that("queries with WHERE and ORDER BY work", {
  mf <- lazy_frame(x = 1, y = 2, con = dialect_db2())
  expect_snapshot(
    mf |>
      filter(x > 0) |>
      arrange(y) |>
      head(5)
  )
})

test_that("custom casts translated correctly", {
  con <- dialect_db2()

  expect_translation(con, as.character(x), 'CAST("x" AS VARCHAR(255))')
  expect_translation(con, as.numeric(x), 'CAST("x" AS DOUBLE)')
  expect_translation(con, as.double(x), 'CAST("x" AS DOUBLE)')
  expect_translation(con, as.integer(x), 'CAST("x" AS INTEGER)')
  expect_translation(con, as.integer64(x), 'CAST("x" AS BIGINT)')
})

test_that("pasting translated correctly", {
  con <- dialect_db2()

  expect_translation(con, paste(x, y), '"x" || \' \' || "y"')
  expect_translation(con, paste0(x, y), '"x" || "y"')
  expect_translation(con, str_c(x, y), '"x" || "y"')
})

test_that("runif uses RAND()", {
  con <- dialect_db2()
  expect_translation(con, runif(n()), "RAND()")
})

test_that("string functions translated correctly", {
  con <- dialect_db2()

  expect_translation(
    con,
    str_replace(x, y, z),
    'REGEXP_REPLACE("x", "y", "z", 1, 1)'
  )
  expect_translation(
    con,
    str_replace_all(x, y, z),
    'REGEXP_REPLACE("x", "y", "z")'
  )
  expect_translation(
    con,
    str_remove(x, y),
    'REGEXP_REPLACE("x", "y", \'\', 1, 1)'
  )
  expect_translation(
    con,
    str_remove_all(x, y),
    'REGEXP_REPLACE("x", "y", \'\')'
  )
  expect_translation(
    con,
    str_squish(x),
    "LTRIM(RTRIM(REGEXP_REPLACE(\"x\", '\\s+', ' ')))"
  )
  expect_translation(con, str_locate(x, "ab"), "LOCATE('ab', \"x\")")
  expect_translation(
    con,
    str_detect(x, fixed("ab")),
    "POSITION('ab' in \"x\") > 0"
  )
  expect_translation(
    con,
    str_starts(x, fixed("ab")),
    "POSITION('ab' in \"x\") = 1"
  )
})

test_that("custom lubridate functions translated correctly", {
  con <- dialect_db2()

  expect_translation(con, today(), "CURRENT DATE")
  expect_translation(con, now(), "CURRENT TIMESTAMP")
  expect_translation(con, yday(x), 'DAYOFYEAR("x")')
  expect_translation(con, quarter(x), 'QUARTER("x")')
  expect_translation(
    con,
    quarter(x, with_year = TRUE),
    "(CHAR(YEAR(\"x\")) || '.' || CHAR(QUARTER(\"x\")))"
  )
  expect_translation(con, week(x), 'WEEK_ISO("x")')
  expect_translation(con, isoweek(x), 'WEEK_ISO("x")')
  expect_translation(con, isoyear(x), 'YEAR("x")')

  expect_snapshot(translate_sql(wday(x), con = con))
  expect_translation(con, wday(x, label = TRUE), 'SUBSTR(DAYNAME("x"), 1, 3)')
  expect_translation(
    con,
    wday(x, label = TRUE, abbr = FALSE),
    'DAYNAME("x")'
  )

  expect_snapshot(
    error = TRUE,
    translate_sql(quarter(x, fiscal_start = 2), con = con)
  )
})

test_that("custom clock functions translated correctly", {
  con <- dialect_db2()

  expect_translation(con, add_days(x, 1L), '("x" + 1 DAYS)')
  expect_translation(con, add_years(x, 1L), '("x" + 1 YEARS)')
  expect_error(
    translate_sql(add_days(x, 1, "must", "be empty"), con = con),
    class = "rlib_error_dots_nonempty"
  )
})

test_that("difftime is translated correctly", {
  con <- dialect_db2()
  expect_translation(
    con,
    difftime(end_date, start_date, units = "days"),
    "(DAYS(CAST(\"end_date\" AS DATE)) - DAYS(CAST(\"start_date\" AS DATE)))"
  )

  expect_snapshot(
    error = TRUE,
    translate_sql(difftime(start_date, end_date, units = "auto"), con = con)
  )
})

test_that("statistical aggregates translated correctly", {
  con <- dialect_db2()

  expect_translation(
    con,
    sd(x, na.rm = TRUE),
    'STDDEV_SAMP("x")',
    window = FALSE
  )
  expect_translation(
    con,
    var(x, na.rm = TRUE),
    'VAR_SAMP("x")',
    window = FALSE
  )
  expect_translation(
    con,
    cor(x, y),
    'CORRELATION("x", "y")',
    window = FALSE
  )
  expect_translation(
    con,
    cov(x, y),
    'COVARIANCE_SAMP("x", "y")',
    window = FALSE
  )
})

test_that("str_flatten uses LISTAGG", {
  con <- dialect_db2()

  expect_translation(
    con,
    str_flatten(x, ", ", na.rm = TRUE),
    "LISTAGG(\"x\", ', ')",
    window = FALSE
  )
  expect_translation(
    con,
    str_flatten(x, ", ", na.rm = TRUE),
    "LISTAGG(\"x\", ', ') OVER ()",
    window = TRUE
  )
})

test_that("sql_table_analyze uses RUNSTATS", {
  con <- dialect_db2()
  expect_snapshot(
    sql_table_analyze(con, ident("mytable"))
  )
})

test_that("copy_inline uses VALUES with column aliases", {
  con <- dialect_db2()
  df <- tibble::tibble(x = 1:2, y = c("a", "b"))
  expect_snapshot(copy_inline(con, df) |> remote_query())
})
