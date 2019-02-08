context("test-backend-impala.R")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_impala())
  }

  expect_equal(trans(as.Date(x)), sql("CAST(`x` AS VARCHAR(10))"))
  expect_equal(trans(ceiling(x)), sql("CEIL(`x`)"))

})

test_that("custom bitwise operations translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_impala())
  }

  expect_equal(trans(bitwNot(x)),        sql("BITNOT(`x`)"))
  expect_equal(trans(bitwAnd(x, 128L)),  sql("BITAND(`x`, 128)"))
  expect_equal(trans(bitwOr(x, 128L)),   sql("BITOR(`x`, 128)"))
  expect_equal(trans(bitwXor(x, 128L)),  sql("BITXOR(`x`, 128)"))
  expect_equal(trans(bitwShiftL(x, 2L)), sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(trans(bitwShiftR(x, 2L)), sql("SHIFTRIGHT(`x`, 2)"))

})
