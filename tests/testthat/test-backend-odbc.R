test_that("custom scalar translated correctly", {
  local_con(simulate_odbc())

  expect_equal(translate_sql(as.numeric(x)),            sql("CAST(`x` AS DOUBLE)"))
  expect_equal(translate_sql(as.double(x)),             sql("CAST(`x` AS DOUBLE)"))
  expect_equal(translate_sql(as.integer(x)),            sql("CAST(`x` AS INT)"))
  expect_equal(translate_sql(as.character(x)),          sql("CAST(`x` AS STRING)"))
})

test_that("custom aggregators translated correctly", {
  local_con(simulate_odbc())

  expect_equal(translate_sql(sd(x), window = FALSE),         sql("STDDEV_SAMP(`x`)"))
  expect_equal(translate_sql(count(), window = FALSE),       sql("COUNT(*)"))
  expect_equal(translate_sql(n(), window = FALSE),           sql("COUNT(*)"))
})

test_that("custom window functions translated correctly", {
  local_con(simulate_odbc())

  expect_equal(translate_sql(sd(x, na.rm = TRUE)), sql("STDDEV_SAMP(`x`) OVER ()"))
  expect_equal(translate_sql(count()),       sql("COUNT(*) OVER ()"))
  expect_equal(translate_sql(n()),           sql("COUNT(*) OVER ()"))
})
