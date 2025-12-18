test_that("rendering table wraps in SELECT *", {
  out <- copy_to(
    test_sqlite(),
    tibble(x = 1),
    name = "test-sql-build",
    overwrite = TRUE
  )
  expect_snapshot(out |> sql_render())
  expect_equal(out |> collect(), tibble(x = 1))
})
