context("test-tbl-lazy")

test_that("tbl_lazy adds src class", {
  tb <- tbl_lazy(mtcars, src = simulate_sqlite())
  expect_s3_class(tb, "tbl_SQLiteConnection")
})

test_that("tbl_lazy has print method", {
  expect_known_output(
    tbl_lazy(mtcars),
    test_path("test-tbl-lazy-print.txt"),
    print = TRUE
  )
})
