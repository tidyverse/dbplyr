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

test_that("doesn't unnecessarily select", {
  mf <- memdb_frame(x = c(3, 1, 2))
  # no warning about select after arrange
  expect_warning(out <- mf %>% arrange(x) %>% pull(), NA)
  expect_equal(out, 1:3)
})

test_that("ungroup() produces nice error messages", {
  expect_snapshot(error = TRUE, {
    memdb_frame(x = 1) %>% pull(non_existent)
    memdb_frame(x = 1) %>% pull(1000)
  })
})

