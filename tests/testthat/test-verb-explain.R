test_that("show_query() prints query", {
  lf <- lazy_frame(x = 1)
  out <- lf |>
    select(x) |>
    filter(x > 0)

  expect_snapshot(show_query(out))
})
