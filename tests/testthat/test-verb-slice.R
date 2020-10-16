test_that("slice, head and tail aren't available", {
  lf <- lazy_frame(x = 1)
  expect_snapshot_error(lf %>% slice())
  expect_snapshot_error(lf %>% slice_head())
  expect_snapshot_error(lf %>% slice_tail())
})

test_that("slice_min handles arguments", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))

  expect_equal(db %>% slice_min(id) %>% pull(), 1)
  expect_equal(db %>% slice_min(x) %>% pull(), c(1, 2))
  expect_equal(db %>% slice_min(x, with_ties = FALSE) %>% pull(), 1)
  expect_equal(db %>% slice_min(id, n = 2) %>% pull(), c(1, 2))
  expect_equal(db %>% slice_min(id, prop = 0.5) %>% pull(), 1)

  expect_snapshot_error(db %>% slice_min())
  expect_snapshot_error(db %>% slice_min(id, prop = 0.5, with_ties = FALSE))
})

test_that("slice_max orders in opposite order", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))

  expect_equal(db %>% slice_max(id) %>% pull(), 3)
  expect_equal(db %>% slice_max(x) %>% pull(), 3)
  expect_snapshot_error(db %>% slice_max())
})

test_that("slice_sample errors when expected", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))

  # Can't see how to test this, but interactive experimentation
  # shows that it doesn't always return the same result
  expect_error(db %>% slice_sample() %>% pull(), NA)

  expect_snapshot_error(db %>% slice_sample(replace = TRUE))
  expect_snapshot_error(db %>% slice_sample(weight_by = x))
  expect_snapshot_error(db %>% slice_sample(prop = 0.5))
})

test_that("window_order is preserved", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))
  sort <- db %>% window_order(x) %>% slice_min(id) %>% op_sort()
  expect_equal(length(sort), 1)
  expect_equal(get_expr(sort[[1]]), quote(x))
})

test_that("check_slice_size checks for common issues", {
  expect_snapshot_error(check_slice_size(n = 1, prop = 1))
  expect_snapshot_error(check_slice_size(n = "a"))
  expect_snapshot_error(check_slice_size(prop = "a"))
  expect_snapshot_error(check_slice_size(n = -1))
  expect_snapshot_error(check_slice_size(prop = -1))
})
