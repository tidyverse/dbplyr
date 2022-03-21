test_that("build_sql() requires connection", {
  x <- ident("TABLE")
  expect_snapshot(error = TRUE, build_sql("SELECT * FROM ", x))
})
