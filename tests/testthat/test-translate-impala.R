context("translate-Impala")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_impala())
  }

  expect_equal(trans(as.Date(x)), sql("CAST(`x` AS VARCHAR(10))"))
  expect_equal(trans(ceiling(x)), sql("CEIL(`x`)"))

})
