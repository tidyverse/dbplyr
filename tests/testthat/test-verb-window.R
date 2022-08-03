test_that("window_order errors for data frame", {
  expect_snapshot({
    (expect_error(window_order(data.frame(x = 1))))
    (expect_error(window_order("a")))
  })
})

test_that("window_frame errors for data frame", {
  expect_snapshot({
    (expect_error(window_frame(data.frame(x = 1))))
  })
})
