context("test-translate-sql-window.r")

test_that("aggregation functions warn if na.rm = FALSE", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))
  sql_mean <- win_aggregate("MEAN")

  expect_warning(sql_mean("x"), "Missing values")
  expect_warning(sql_mean("x", na.rm = TRUE), NA)
})
