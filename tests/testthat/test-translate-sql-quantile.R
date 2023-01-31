test_that("quantile and median don't change without warning", {
  expect_snapshot(translate_sql(quantile(x, 0.75, na.rm = TRUE), window = FALSE))
  expect_snapshot(translate_sql(quantile(x, 0.75, na.rm = TRUE), vars_group = "g"))
  expect_snapshot(translate_sql(median(x, na.rm = TRUE), window = FALSE))
  expect_snapshot(translate_sql(median(x, na.rm = TRUE), vars_group = "g"))
})

test_that("checks for invalid probs", {
  expect_error(check_probs("a"), "number")
  expect_error(check_probs(1:3), "vector")
})
