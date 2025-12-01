test_that("aggregation functions warn once if na.rm = FALSE", {
  skip_on_cran()
  env_unbind(ns_env("rlang")$warning_freq_env, "dbplyr_check_na_rm")

  local_con(simulate_dbi())
  sql_mean <- sql_aggregate("MEAN")

  expect_warning(sql_mean("x", na.rm = TRUE), NA)
  expect_warning(sql_mean("x"), "Missing values")
  expect_warning(sql_mean("x"), NA)
})

test_that("warns informatively with unsupported function", {
  expect_snapshot(error = TRUE, sql_not_supported("cor")())
})
