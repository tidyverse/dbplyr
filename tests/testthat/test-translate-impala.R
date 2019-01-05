context("translate-Impala")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_impala())
  }

  expect_equal(trans(as.Date(x)), sql("CAST(`x` AS VARCHAR(10))"))
  expect_equal(trans(ceiling(x)), sql("CEIL(`x`)"))

})

mf <- lazy_frame(x = 1, src = simulate_impala())

test_that("sample_frac() returns the correct query", {
  expect_equal(
    mf %>% sample_frac(0.1) %>% show_query(),
    sql("SELECT *\nFROM `df`\nTABLESAMPLE SYSTEM (10)")
  )
})

test_that("sample_n() returns the expected error message", {
  expect_error(
    mf %>% sample_n(10) %>% show_query(),
    "Explicit row size sample is not supported"
  )
})
