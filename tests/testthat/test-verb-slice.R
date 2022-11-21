test_that("slice, head and tail aren't available", {
  lf <- lazy_frame(x = 1)
  expect_snapshot(error = TRUE, lf %>% slice())
  expect_snapshot(error = TRUE, lf %>% slice_head())
  expect_snapshot(error = TRUE, lf %>% slice_tail())
})

test_that("slice_min handles arguments", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))

  expect_equal(db %>% slice_min(id) %>% pull(), 1)
  expect_equal(db %>% slice_min(x) %>% pull(), c(1, 2))
  expect_equal(db %>% slice_min(x, with_ties = FALSE) %>% pull(), 1)
  expect_equal(db %>% slice_min(id, n = 2) %>% pull(), c(1, 2))
  expect_equal(db %>% slice_min(id, prop = 0.5) %>% pull(), 1)

  expect_snapshot(error = TRUE, db %>% slice_min())
  expect_snapshot(error = TRUE, db %>% slice_min(id, prop = 0.5, with_ties = FALSE))
})

test_that("slice_max orders in opposite order", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))

  expect_equal(db %>% slice_max(id) %>% pull(), 3)
  expect_equal(db %>% slice_max(x) %>% pull(), 3)
  expect_snapshot(error = TRUE, db %>% slice_max())
})

test_that("slice_sample errors when expected", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))

  # Can't see how to test this, but interactive experimentation
  # shows that it doesn't always return the same result
  expect_error(db %>% slice_sample() %>% pull(), NA)

  expect_snapshot(error = TRUE, db %>% slice_sample(replace = TRUE))
  expect_snapshot(error = TRUE, db %>% slice_sample(weight_by = x))
  expect_snapshot(error = TRUE, db %>% slice_sample(prop = 0.5))
})

test_that("window_order is preserved", {
  db <- memdb_frame(x = c(1, 1, 2), id = c(1, 2, 3))
  sort <- db %>% window_order(x) %>% slice_min(id) %>% op_sort()
  expect_equal(length(sort), 1)
  expect_equal(get_expr(sort[[1]]), quote(x))
})

test_that("check_slice_size checks for common issues", {
  lf <- lazy_frame(x = c(1, 1, 2), id = c(1, 2, 3))

  expect_snapshot(error = TRUE, lf %>% slice_sample(n = 1, prop = 1))
  expect_snapshot(error = TRUE, lf %>% slice_sample(n = "a"))
  expect_snapshot(error = TRUE, lf %>% slice_sample(prop = "a"))
  expect_snapshot(error = TRUE, lf %>% slice_sample(n = -1))
  expect_snapshot(error = TRUE, lf %>% slice_sample(prop = -1))
})

test_that("catches `.by` with grouped-df", {
  df <- tibble(x = 1)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    slice(gdf, .by = x)
  })
})

test_that("slice_helper `by` errors use correct error context and correct `by_arg`", {
  df <- lazy_frame(x = 1)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    slice_min(gdf, order_by = x, by = x)
    slice_max(gdf, order_by = x, by = x)
    slice_sample(gdf, n = 1, by = x)
  })
})

test_that("slice_min/max() work with `by`", {
  df <- memdb_frame(g = c(2, 2, 1, 1), x = c(1, 2, 3, 1))

  out <- slice_min(df, x, by = g) %>% arrange(g) %>% collect()
  expect_identical(out, tibble(g = c(1, 2), x = 1))
  expect_equal(group_vars(out), character())
  expect_identical(slice_max(df, x, by = g) %>% arrange(g) %>% collect(), tibble(g = c(1, 2), x = c(3, 2)))
})

test_that("slice_sample() works with `by`", {
  df <- tibble(g = c(2, 2, 2, 1), x = c(1, 2, 3, 1))
  df <- memdb_frame(g = c(2, 2, 2, 1), x = c(1, 2, 3, 1))
  expect_identical(
    slice_sample(df, n = 2, by = g) %>% pull(g) %>% sort(),
    c(1, 2, 2)
  )
})
