context("test-src_dbi.R")

test_that("tbl and src classes include connection class", {

  mf <- memdb_frame(x = 1, y = 2)
  expect_true(inherits(mf, "tbl_SQLiteConnection"))
  expect_true(inherits(mf$src, "src_SQLiteConnection"))
})
