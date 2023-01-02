test_that("can print lazy_select_query", {
  expect_snapshot(
    lazy_select_query(
      x = lazy_query_local(tibble(x = 1, y = 2), "df"),
      select = quos(
        x_mean = mean(x),
        y2 = y
      ),
      where = quos(y > 1, x == y - 2),
      group_by = quos("x")
    )
  )
})
