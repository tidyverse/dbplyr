context("pull")

test_that("default extracts last var from remote source", {
  mf <- memdb_frame(x = 1:10, y = 1:10)
  expect_equal(pull(mf), 1:10)
})

test_that("can extract by name, or positive/negative position", {
  x <- 1:10
  mf <- memdb_frame(x, y = runif(10))

  expect_equal(pull(mf, x), x)
  expect_equal(pull(mf, 1L), x)
  expect_equal(pull(mf, 1), x)
  expect_equal(pull(mf, -2), x)
  expect_equal(pull(mf, -2L), x)
})
