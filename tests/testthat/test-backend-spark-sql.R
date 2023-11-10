test_that("custom clock functions translated correctly", {
  local_con(simulate_spark_sql())
  expect_equal(test_translate_sql(add_years(x, 1)), sql("ADD_MONTHS('`x`', 1.0 * 12.0)"))
  expect_equal(test_translate_sql(add_days(x, 1)), sql("DATE_ADD(`x`, 1.0)"))
  expect_error(test_translate_sql(add_days(x, 1, "dots", "must", "be empty")))
})

test_that("difftime is translated correctly", {
  local_con(simulate_spark_sql())
  expect_equal(test_translate_sql(difftime(start_date, end_date, units = "days")), sql("DATEDIFF(`end_date`, `start_date`)"))
  expect_equal(test_translate_sql(difftime(start_date, end_date)), sql("DATEDIFF(`end_date`, `start_date`)"))

  expect_error(test_translate_sql(difftime(start_date, end_date, units = "auto")))
  expect_error(test_translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days")))
})
