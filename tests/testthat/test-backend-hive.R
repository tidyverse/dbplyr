context("test-backend-hive.R")

test_that("custom scalar & string functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_hive())
  }

  expect_equal(trans(bitwShiftL(x, 2L)),                sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(trans(bitwShiftR(x, 2L)),                sql("SHIFTRIGHT(`x`, 2)"))
  expect_equal(trans(cot(x)),                           sql("1.0 / TAN(`x`)"))
  expect_equal(trans(str_replace_all(x, "old", "new")), sql("REGEXP_REPLACE(`x`, 'old', 'new')"))
  expect_equal(trans(median(x)),                        sql("PERCENTILE(`x`, 0.5) OVER ()"))

})


