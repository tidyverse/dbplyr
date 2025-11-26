test_that("show_sql() prints query and returns self", {
  lf <- lazy_frame(x = 1)
  out <- lf %>%
    select(x) %>%
    filter(x > 0)

  expect_snapshot(show_sql(out))
  expect_identical(show_sql(out), out)
  expect_invisible(show_sql(out))
})

test_that("show_sql() can produce CTEs", {
  lf <- lazy_frame(x = 1, y = 2)
  out <- lf %>%
    mutate(z = x + y) %>%
    left_join(lf, by = c("x", "y"))

  expect_snapshot(show_sql(out, cte = TRUE))
})

test_that("show_sql() passes along sql_options", {
  con <- simulate_sqlite()
  lf <- lazy_frame(x = 1, con = con)

  expect_snapshot(
    show_sql(lf, sql_options = sql_options(use_star = FALSE))
  )
})


test_that("explain() prints query and plan", {
  mf <- memdb_frame(x = 1)
  out <- mf %>%
    select(x) %>%
    filter(x > 0)

  expect_snapshot(explain(out))
  expect_identical(explain(out), out)
  expect_invisible(explain(out))
})
