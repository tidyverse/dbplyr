context("test-translate-sql-quantile")

test_that("quantile and median don't change without warning", {
  reg <- list(
    quantile = translate_sql(quantile(x, 0.75), window = FALSE),
    quantile_win = translate_sql(quantile(x, 0.75), vars_group = "g"),
    median = translate_sql(median(x), window = FALSE),
    median_win = translate_sql(median(x), vars_group = "g")
  )

  expect_known_output(print(reg), test_path("sql/backend-quantile.sql"))
})

test_that("checks for invalid probs", {
  expect_error(check_probs("a"), "numeric")
  expect_error(check_probs(1:3), "single value")
})
