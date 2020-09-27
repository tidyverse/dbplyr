test_that("rendering table wraps in SELECT *", {
  out <- memdb_frame(x = 1, .name = "test-sql-build")
  expect_snapshot(out %>% sql_render())
  expect_equal(out %>% collect(), tibble(x = 1))
})
