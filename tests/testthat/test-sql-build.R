context("test-sql-build")

test_that("rendering table wraps in SELECT *", {
  out <- memdb_frame(x = 1)
  expect_match(out %>% sql_render(), "^SELECT [*]\nFROM `[^`]*`$")
  expect_equal(out %>% collect(), tibble(x = 1))
})
