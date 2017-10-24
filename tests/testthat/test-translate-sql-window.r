context("test-translate-sql-window.r")

test_that("aggregation functions warn if na.rm = FALSE", {
  sql_mean <- win_aggregate("mean")

  expect_warning(sql_mean("x"), "Missing values")
  expect_warning(sql_mean("x", na.rm = TRUE), NA)
})
