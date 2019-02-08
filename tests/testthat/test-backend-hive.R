context("test-backend-hive.R")

test_that("custom scalar & string functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_hive())
  }

  expect_equal(trans(bitwShiftL(x, 2L)),                sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(trans(bitwShiftR(x, 2L)),                sql("SHIFTRIGHT(`x`, 2)"))
  expect_equal(trans(cot(x)),                           sql("1.0 / TAN(`x`)"))
  expect_equal(trans(str_replace_all(x, "old", "new")), sql("REGEXP_REPLACE(`x`, 'old', 'new')"))

})

test_that("Sampling functions return their respective expected query", {
  mf <- lazy_frame(x = 1, src = simulate_hive())

  expect_equal(
    mf %>% sample_n(10) %>% show_query(),
    sql("SELECT *\nFROM `df`\nTABLESAMPLE(10 ROWS)")
  )

  expect_equal(
    mf %>% sample_frac(0.1) %>% show_query(),
    sql("SELECT *\nFROM `df`\nTABLESAMPLE(10 PERCENT)")
  )
})
