context("translate-sqlite")

test_that("vectorised translations", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_sqlite(), window = FALSE)
  }

  expect_equal(trans(nullif(x, 0L)), sql('NULLIF(`x`, 0)'))
  expect_equal(trans(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(trans(paste0(x, y)), sql("`x` || `y`"))
})
