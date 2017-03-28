context("pull")

test_that("default extracts last var from remote source", {
  mf <- memdb_frame(x = 1:10, y = 1:10)
  expect_equal(pull(mf), 1:10)
})
