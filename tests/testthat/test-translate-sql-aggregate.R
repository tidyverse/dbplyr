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

test_that("quantile and median don't change without warning", {
  local_con(simulate_dbi())
  expect_snapshot(test_translate_sql(
    quantile(x, 0.75, na.rm = TRUE),
    window = FALSE
  ))
  expect_snapshot(test_translate_sql(
    quantile(x, 0.75, na.rm = TRUE),
    vars_group = "g"
  ))
  expect_snapshot(test_translate_sql(median(x, na.rm = TRUE), window = FALSE))
  expect_snapshot(test_translate_sql(median(x, na.rm = TRUE), vars_group = "g"))
})

test_that("checks for invalid probs", {
  expect_error(check_probs("a"), "number")
  expect_error(check_probs(1:3), "vector")
})
