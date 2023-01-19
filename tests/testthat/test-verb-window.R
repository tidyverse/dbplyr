test_that("window_order errors for data frame", {
  expect_snapshot({
    (expect_error(window_order(data.frame(x = 1))))
    (expect_error(window_order("a")))
  })
})

test_that("window_order only accepts variables", {
  lf <- lazy_frame(x = 1, y = 1)
  expect_equal(window_order(lf, x, y) %>% op_sort(), unname(exprs(x, y)))
  expect_equal(window_order(lf, x, desc(y)) %>% op_sort(), unname(exprs(x, desc(y))))

  expect_snapshot({
    (expect_error(window_order(lf, x + y)))
  })
})

test_that("window_frame errors for data frame", {
  expect_snapshot({
    (expect_error(window_frame(data.frame(x = 1))))
  })
})
