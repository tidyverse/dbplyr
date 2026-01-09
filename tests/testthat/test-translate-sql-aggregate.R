test_that("aggregation functions warn once if na.rm = FALSE", {
  con <- simulate_dbi()
  reset_warning_verbosity("dbplyr_check_na_rm")
  local_mocked_bindings(is_testing = function() FALSE)

  expect_no_warning(translate_sql(mean(x, na.rm = TRUE), con = con))
  expect_warning(
    translate_sql(mean(x, na.rm = FALSE), con = con),
    "Missing values"
  )
  expect_no_warning(translate_sql(mean(x, na.rm = FALSE), con = con))
})

test_that("warns informatively with unsupported function", {
  expect_snapshot(error = TRUE, sql_not_supported("cor")())
})

test_that("quantile and median don't change without warning", {
  con <- simulate_dbi()
  expect_snapshot(translate_sql(
    quantile(x, 0.75, na.rm = TRUE),
    con = con,
    window = FALSE
  ))
  expect_snapshot(translate_sql(
    quantile(x, 0.75, na.rm = TRUE),
    con = con,
    vars_group = "g"
  ))
  expect_snapshot(translate_sql(
    median(x, na.rm = TRUE),
    con = con,
    window = FALSE
  ))
  expect_snapshot(translate_sql(
    median(x, na.rm = TRUE),
    con = con,
    vars_group = "g"
  ))
})

test_that("checks for invalid probs", {
  expect_error(check_probs("a"), "number")
  expect_error(check_probs(1:3), "vector")
})
