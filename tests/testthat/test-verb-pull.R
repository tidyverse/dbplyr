context("test-verb-pull")

test_that("can extract default, by name, or positive/negative position", {
  x <- 1:10
  y <- runif(10)
  mf <- memdb_frame(x = x, y = y)

  expect_equal(pull(mf), y)
  expect_equal(pull(mf, x), x)
  expect_equal(pull(mf, 1L), x)
  expect_equal(pull(mf, -1), y)
})

test_that("extracts correct column from grouped tbl", {
  mf <- memdb_frame(id = "a", value = 42)
  gf <- mf %>% group_by(id)

  expect_equal(pull(mf, value), 42)
})
