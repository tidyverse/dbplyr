test_that("defaults to postgres translations", {
  local_con(simulate_redshift())
  expect_equal(test_translate_sql(log10(x)), sql("LOG(`x`)"))
})

test_that("string translations", {
  local_con(simulate_redshift())

  expect_error(test_translate_sql(str_replace("xx", ".", "a")), "not available")
  expect_equal(test_translate_sql(str_replace_all("xx", ".", "a")), sql("REGEXP_REPLACE('xx', '.', 'a')"))

  expect_equal(test_translate_sql(substr(x, 2, 2)), sql("SUBSTRING(`x`, 2, 1)"))
  expect_equal(test_translate_sql(str_sub(x, 2, -2)), sql("SUBSTRING(`x`, 2, LEN(`x`) - 2)"))

  expect_equal(test_translate_sql(paste("x", "y")), sql("'x' || ' ' || 'y'"))
  expect_equal(test_translate_sql(paste0("x", "y")), sql("'x' || 'y'"))
  expect_equal(test_translate_sql(str_c("x", "y")), sql("'x' || 'y'"))
})

test_that("numeric translations", {
  local_con(simulate_redshift())

  expect_equal(test_translate_sql(as.numeric(x)), sql("CAST(`x` AS FLOAT)"))
  expect_equal(test_translate_sql(as.double(x)), sql("CAST(`x` AS FLOAT)"))
  expect_equal(test_translate_sql(round(1.234, 1)), sql("ROUND((1.234) :: float, 1)"))
})

test_that("aggregate functions", {
  local_con(simulate_redshift())

  expect_equal(test_translate_sql(str_flatten(x, y), window = FALSE), sql("LISTAGG(`x`, `y`)"))
  expect_equal(test_translate_sql(str_flatten(x, y), window = TRUE), sql("LISTAGG(`x`, `y`) OVER ()"))
  expect_equal(test_translate_sql(order_by(z, str_flatten(x, y))), sql("LISTAGG(`x`, `y`) WITHIN GROUP (ORDER BY `z`) OVER ()"))
})

test_that("lag and lead translation", {
  local_con(simulate_redshift())

  expect_equal(test_translate_sql(lead(x)), sql("LEAD(`x`, 1) OVER ()"))
  expect_equal(test_translate_sql(lag(x)), sql("LAG(`x`, 1) OVER ()"))

  expect_error(test_translate_sql(lead(x, default = y)), "unused argument")
  expect_error(test_translate_sql(lag(x, default = y)), "unused argument")
})

test_that("copy_inline uses UNION ALL", {
  con <- simulate_redshift()
  y <- tibble::tibble(id = 1L, arr = "{1,2,3}")

  types <- c(id = "bigint", arr = "integer[]")
  expect_snapshot({
    copy_inline(con, y %>% slice(0)) %>% remote_query()
    copy_inline(con, y) %>% remote_query()

    # with `types`
    copy_inline(con, y %>% slice(0), types = types) %>% remote_query()
    copy_inline(con, y, types = types) %>% remote_query()
  })
})

test_that("custom clock functions translated correctly", {
  local_con(simulate_redshift())
  expect_equal(test_translate_sql(add_years(x, 1)), sql("DATEADD(YEAR, 1.0, `x`)"))
  expect_equal(test_translate_sql(add_days(x, 1)), sql("DATEADD(DAY, 1.0, `x`)"))
  expect_error(test_translate_sql(add_days(x, 1, "dots", "must", "be empty")))
  expect_equal(test_translate_sql(date_build(2020, 1, 1)), sql("TO_DATE(CAST(2020.0 AS TEXT) || '-' CAST(1.0 AS TEXT) || '-' || CAST(1.0 AS TEXT)), 'YYYY-MM-DD')"))
  expect_equal(test_translate_sql(date_build(year_column, 1L, 1L)), sql("TO_DATE(CAST(`year_column` AS TEXT) || '-' CAST(1 AS TEXT) || '-' || CAST(1 AS TEXT)), 'YYYY-MM-DD')"))
  expect_equal(test_translate_sql(get_year(date_column)), sql("DATE_PART('year', `date_column`)"))
  expect_equal(test_translate_sql(get_month(date_column)), sql("DATE_PART('month', `date_column`)"))
  expect_equal(test_translate_sql(get_day(date_column)), sql("DATE_PART('day', `date_column`)"))
  expect_equal(test_translate_sql(date_count_between(date_column_1, date_column_2, "day")),
               sql("DATEDIFF(DAY, `date_column_1`, `date_column_2`)"))
  expect_error(test_translate_sql(date_count_between(date_column_1, date_column_2, "year")))
  expect_error(test_translate_sql(date_count_between(date_column_1, date_column_2, "day", n = 5)))
})

test_that("difftime is translated correctly", {
  local_con(simulate_redshift())
  expect_equal(test_translate_sql(difftime(start_date, end_date, units = "days")), sql("DATEDIFF(DAY, `start_date`, `end_date`)"))
  expect_equal(test_translate_sql(difftime(start_date, end_date)), sql("DATEDIFF(DAY, `start_date`, `end_date`)"))

  expect_error(test_translate_sql(difftime(start_date, end_date, units = "auto")))
  expect_error(test_translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days")))
})
