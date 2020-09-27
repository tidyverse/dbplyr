test_that("custom string translations", {
  local_con(simulate_hana())

  expect_snapshot(translate_sql(paste0("a", "b")))
  expect_snapshot(translate_sql(paste("a", "b")))

  expect_snapshot(translate_sql(substr(x, 2, 4)))
  expect_snapshot(translate_sql(substring(x, 2, 4)))
  expect_snapshot(translate_sql(str_sub(x, 2, -2)))
})
