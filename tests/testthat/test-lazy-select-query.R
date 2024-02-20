test_that("can print lazy_select_query", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(
    lazy_select_query(
      x = lf$lazy_query,
      select = quos(
        x_mean = mean(x),
        y2 = y
      ),
      where = quos(y > 1, x == y - 2),
      group_by = quos("x")
    )
  )
})
