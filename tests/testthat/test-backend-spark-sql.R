test_that("str_like and str_ilike work", {
  con <- simulate_spark_sql()
  expect_translation(con, str_like(x, "y"), '"x" LIKE \'y\'')
  expect_translation(con, str_ilike(x, "y"), '"x" ILIKE \'y\'')
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_translation(con, str_like(x, y, ignore_case = TRUE), '"x" ILIKE "y"')
})

test_that("custom clock functions translated correctly", {
  con <- simulate_spark_sql()
  expect_translation(con, add_years(x, 1), 'ADD_MONTHS("x", 1.0 * 12)')
  expect_translation(con, add_days(x, 1), 'DATE_ADD("x", 1.0)')
  expect_error(
    translate_sql(add_days(x, 1, "dots", "must", "be empty"), con = con),
    class = "rlib_error_dots_nonempty"
  )
  expect_translation(con, date_build(2020L, 1L, 1L), "MAKE_DATE(2020, 1, 1)")
  expect_translation(con, date_build(x, 1L, 1L), 'MAKE_DATE("x", 1, 1)')
  expect_translation(con, get_year(x), 'DATE_PART(\'YEAR\', "x")')
  expect_translation(con, get_month(x), 'DATE_PART(\'MONTH\', "x")')
  expect_translation(con, get_day(x), 'DATE_PART(\'DAY\', "x")')
  expect_translation(con, date_count_between(x, y, "day"), 'DATEDIFF("y", "x")')
  expect_translation_error(con, date_count_between(x, y, "year"))
  expect_translation_error(con, date_count_between(x, y, "day", n = 5))
})

test_that("difftime is translated correctly", {
  con <- simulate_spark_sql()
  expect_translation(con, difftime(x, y, units = "days"), 'DATEDIFF("y", "x")')
  expect_translation(con, difftime(x, y), 'DATEDIFF("y", "x")')
  expect_translation_error(con, difftime(x, y, units = "auto"))
  expect_translation_error(con, difftime(x, y, tz = "UTC"))
})

test_that("first and last aggregate functions work", {
  con <- simulate_spark_sql()
  expect_translation(con, first(x), 'FIRST("x")', window = FALSE)
  expect_translation(con, last(x), 'LAST("x")', window = FALSE)
  expect_translation_error(con, nth(x, 2), 'LAST("x")', window = FALSE)
})

test_that("first, last, and nth window functions work", {
  con <- simulate_spark_sql()
  expect_translation(
    con,
    first(x, na_rm = TRUE),
    'FIRST_VALUE("x", TRUE) OVER (ORDER BY "a")',
    vars_order = "a"
  )
  expect_translation(
    con,
    last(x, na_rm = TRUE),
    'LAST_VALUE("x", TRUE) OVER (ORDER BY "a" ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)',
    vars_order = "a"
  )
  expect_translation(
    con,
    nth(x, 2, na_rm = TRUE),
    'NTH_VALUE("x", 2, TRUE) OVER (ORDER BY "a")',
    vars_order = "a"
  )
})

test_that("generates custom sql", {
  con <- simulate_spark_sql()
  expect_equal(
    sql_table_analyze(con, in_schema("schema", "tbl")),
    sql('ANALYZE TABLE "schema"."tbl" COMPUTE STATISTICS')
  )
  expect_true(supports_window_clause(con))
})

test_that("db_copy_to and db_compute generate correct SQL", {
  con <- simulate_spark_sql()

  executed_sql <- NULL
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      executed_sql <<- statement
      0L
    },
    .package = "DBI"
  )

  db_compute(con, "t", sql("SELECT 1"), temporary = TRUE)
  expect_match(executed_sql, "CREATE TEMPORARY VIEW")

  db_compute(con, "t", sql("SELECT 1"), temporary = FALSE)
  expect_match(executed_sql, "CREATE TABLE")

  db_compute(con, "t", sql("SELECT 1"), overwrite = TRUE)
  expect_match(executed_sql, "CREATE OR REPLACE")

  db_copy_to(con, "t", data.frame(x = 1))
  expect_match(executed_sql, "CREATE TEMPORARY VIEW")
})
