context("test-backend-odbc.R")

test_that("custom scalar translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_odbc())
  }

  expect_equal(trans(as.numeric(x)),            sql("CAST(`x` AS DOUBLE)"))
  expect_equal(trans(as.double(x)),             sql("CAST(`x` AS DOUBLE)"))
  expect_equal(trans(as.integer(x)),            sql("CAST(`x` AS INT)"))
  expect_equal(trans(as.character(x)),          sql("CAST(`x` AS STRING)"))
})

test_that("custom aggregators translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = FALSE, con = simulate_odbc())
  }

  expect_equal(trans(sd(x)),         sql("STDDEV_SAMP(`x`)"))
  expect_equal(trans(count()),       sql("COUNT(*)"))
  expect_equal(trans(n()),           sql("COUNT(*)"))
})

test_that("custom window functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), window = TRUE, con = simulate_odbc())
  }

  expect_equal(trans(sd(x, na.rm = TRUE)),         sql("STDDEV_SAMP(`x`) OVER ()"))
  expect_equal(trans(count()),       sql("COUNT(*) OVER ()"))
  expect_equal(trans(n()),           sql("COUNT(*) OVER ()"))
})
