test_that("show_sql() prints query and returns self", {
  lf <- lazy_frame(x = 1)
  out <- lf |>
    select(x) |>
    filter(x > 0)

  expect_snapshot(show_sql(out))
  expect_identical(show_sql(out), out)
  expect_invisible(show_sql(out))
})

test_that("show_sql() can produce CTEs", {
  lf <- lazy_frame(x = 1, y = 2)
  out <- lf |>
    mutate(z = x + y) |>
    left_join(lf, by = c("x", "y"))

  expect_snapshot(show_sql(out, cte = TRUE))
})
