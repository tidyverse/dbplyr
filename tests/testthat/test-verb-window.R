# ops ---------------------------------------------------------------------

test_that("order is retained after head() (#373)", {
  mf <- lazy_frame(x = c(2, 2, 1), y = c(1, -1, 1))

  out <- mf %>% window_order(y) %>% head(1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list(quote(y)))
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

  out <- mf %>% window_order(y) %>% summarise(x = sum(x))
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out), NA)

  out <- mf %>% window_order(y) %>% filter(x < 1)
  sort <- lapply(op_sort(out), get_expr)
  expect_equal(sort, list())
  expect_warning(sql_render(out), NA)
})
