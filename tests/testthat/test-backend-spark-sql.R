test_that("custom clock functions translated correctly", {
  con <- dialect_spark_sql()
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
  con <- dialect_spark_sql()
  expect_translation(
    con,
    difftime(start_date, end_date, units = "days"),
    'DATEDIFF("end_date", "start_date")'
  )
  expect_translation(
    con,
    difftime(start_date, end_date),
    'DATEDIFF("end_date", "start_date")'
  )

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
