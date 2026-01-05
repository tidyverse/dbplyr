test_that("rendering table wraps in SELECT *", {
  out <- local_memdb_frame("test-sql-build", x = 1)
  expect_snapshot(out |> sql_render())
  expect_equal(out |> collect(), tibble(x = 1))
})
