test_that("custom clock functions translated correctly", {
  local_con(simulate_spark_sql())
  expect_equal(test_translate_sql(add_years(x, 1)), sql("ADD_MONTHS(`x`, 1.0 * 12.0)"))
  expect_equal(test_translate_sql(add_days(x, 1)), sql("DATE_ADD(`x`, 1.0)"))
  expect_error(test_translate_sql(add_days(x, 1, "dots", "must", "be empty")))
  expect_equal(test_translate_sql(date_build(2020, 1, 1)), sql("MAKE_DATE(2020.0, 1.0, 1.0)"))
  expect_equal(test_translate_sql(date_build(year_column, 1L, 1L)), sql("MAKE_DATE(`year_column`, 1, 1)"))
  expect_equal(test_translate_sql(get_year(date_column)), sql("DATE_PART('YEAR', `date_column`)"))
  expect_equal(test_translate_sql(get_month(date_column)), sql("DATE_PART('MONTH', `date_column`)"))
  expect_equal(test_translate_sql(get_day(date_column)), sql("DATE_PART('DAY', `date_column`)"))
  expect_equal(test_translate_sql(date_count_between(date_column_1, date_column_2, "day")),
               sql("DATEDIFF(`date_column_2`, `date_column_1`)"))
  expect_error(test_translate_sql(date_count_between(date_column_1, date_column_2, "year")))
  expect_error(test_translate_sql(date_count_between(date_column_1, date_column_2, "day", n = 5)))
})

test_that("difftime is translated correctly", {
  local_con(simulate_spark_sql())
  expect_equal(test_translate_sql(difftime(start_date, end_date, units = "days")), sql("DATEDIFF(`end_date`, `start_date`)"))
  expect_equal(test_translate_sql(difftime(start_date, end_date)), sql("DATEDIFF(`end_date`, `start_date`)"))

  expect_error(test_translate_sql(difftime(start_date, end_date, units = "auto")))
  expect_error(test_translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days")))
})
