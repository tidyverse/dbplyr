test_that("str_like and str_ilike work", {
  con <- simulate_spark_sql()
  expect_translation(con, str_like(x, "y"), '"x" LIKE \'y\'')
  expect_translation(con, str_ilike(x, "y"), '"x" ILIKE \'y\'')
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_translation(con, str_like(x, y, ignore_case = TRUE), '"x" ILIKE "y"')
})

test_that("custom clock functions translated correctly", {
  con <- simulate_spark_sql()
  expect_translation(
    con,
    add_years(x, 1),
    'ADD_MONTHS("x", 1.0 * 12)'
  )
  expect_translation(con, add_days(x, 1), 'DATE_ADD("x", 1.0)')
  expect_error(
    translate_sql(add_days(x, 1, "dots", "must", "be empty"), con = con),
    class = "rlib_error_dots_nonempty"
  )
  expect_translation(
    con,
    date_build(2020, 1, 1),
    "MAKE_DATE(2020.0, 1.0, 1.0)"
  )
  expect_translation(
    con,
    date_build(year_column, 1L, 1L),
    'MAKE_DATE("year_column", 1, 1)'
  )
  expect_translation(
    con,
    get_year(date_column),
    'DATE_PART(\'YEAR\', "date_column")'
  )
  expect_translation(
    con,
    get_month(date_column),
    'DATE_PART(\'MONTH\', "date_column")'
  )
  expect_translation(
    con,
    get_day(date_column),
    'DATE_PART(\'DAY\', "date_column")'
  )
  expect_translation(
    con,
    date_count_between(date_column_1, date_column_2, "day"),
    'DATEDIFF("date_column_2", "date_column_1")'
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(
      date_count_between(date_column_1, date_column_2, "year"),
      con = con
    )
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(
      date_count_between(
        date_column_1,
        date_column_2,
        "day",
        n = 5
      ),
      con = con
    )
  )
})

test_that("difftime is translated correctly", {
  con <- simulate_spark_sql()
  expect_translation(
    con,
    difftime(start_date, end_date, units = "days"),
    'DATEDIFF("end_date", "start_date")'
  )
  expect_translation(con, difftime(start, end), 'DATEDIFF("end", "start")')

  expect_snapshot(
    error = TRUE,
    translate_sql(difftime(start_date, end_date, units = "auto"), con = con)
  )
  expect_snapshot(
    error = TRUE,
    translate_sql(
      difftime(
        start_date,
        end_date,
        tz = "UTC",
        units = "days"
      ),
      con = con
    )
  )
})

test_that("first and last aggregate functions work", {
  con <- simulate_spark_sql()
  expect_translation(con, first(x), 'FIRST("x")', window = FALSE)
  expect_translation(con, last(x), 'LAST("x")', window = FALSE)
})

test_that("first, last, and nth window functions work", {
  con <- simulate_spark_sql()
  expect_equal(
    translate_sql(first(x, na_rm = TRUE), vars_order = "a", con = con),
    sql('FIRST_VALUE("x", TRUE) OVER (ORDER BY "a")')
  )
  expect_equal(
    translate_sql(last(x, na_rm = TRUE), vars_order = "a", con = con),
    sql(
      'LAST_VALUE("x", TRUE) OVER (ORDER BY "a" ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)'
    )
  )
  expect_equal(
    translate_sql(nth(x, 2, na_rm = TRUE), vars_order = "a", con = con),
    sql('NTH_VALUE("x", 2, TRUE) OVER (ORDER BY "a")')
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

  db_compute(con, "test_table", sql("SELECT 1"), temporary = TRUE)
  expect_match(executed_sql, "CREATE TEMPORARY VIEW")

  db_compute(con, "test_table", sql("SELECT 1"), temporary = FALSE)
  expect_match(executed_sql, "CREATE TABLE")

  db_compute(con, "test_table", sql("SELECT 1"), overwrite = TRUE)
  expect_match(executed_sql, "CREATE OR REPLACE")

  db_copy_to(con, "test_table", data.frame(x = 1))
  expect_match(executed_sql, "CREATE TEMPORARY VIEW")
})
