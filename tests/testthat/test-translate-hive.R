context("translate-Hive")

test_that("custom scalar & string functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_hive())
  }

  expect_equal(trans(cot(x)),                           sql("1.0 / TAN(`x`)"))
  expect_equal(trans(str_replace_all(x, "old", "new")), sql("REGEXP_REPLACE(`x`, 'old', 'new')"))

})


