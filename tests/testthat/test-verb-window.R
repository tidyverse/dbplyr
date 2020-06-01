# ops ---------------------------------------------------------------------

test_that("order is retained after head() (#373)", {
  mf <- lazy_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  out <- mf %>% window_order(y) %>% head(1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
})

test_that("order overrides silently (#373)", {
  out <- window_order(window_order(lazy_frame(x = 1:3, y = 3:1), x), y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
  expect_warning(sql_render(out), NA)
})

test_that("order and frame coexist (#373)", {
  out <- window_order(window_frame(lazy_frame(x = 1:3, y = 3:1), 0, 0), y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
  frame <- op_frame(out)
  expect_identical(frame, c(0, 0))
  expect_warning(sql_render(out), NA)

  out <- window_frame(window_order(lazy_frame(x = 1:3, y = 3:1), x), 0, 0)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(x)))
  frame <- op_frame(out)
  expect_identical(frame, c(0, 0))
  expect_warning(sql_render(out), NA)
})

test_that("order is lost after verbs, without warning (#373)", {
  mf <- lazy_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  out <- mf %>% window_order(y) %>% select(-y)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out), NA)

  out <- mf %>% window_order(y) %>% mutate(y = 1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out), NA)

  out <- mf %>% window_order(y) %>% summarise(x = sum(x, na.rm = TRUE))
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out), NA)

  out <- mf %>% window_order(y) %>% filter(x < 1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out), NA)
})
