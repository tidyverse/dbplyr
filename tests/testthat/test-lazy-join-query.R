test_that("sql_build.lazy_multi_join_query() includes distinct", {
  lf1 <- lazy_frame(x = 1, y = 1)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- lf1 |>
    left_join(lf2, by = "x") |>
    distinct()

  query <- out$lazy_query
  expect_s3_class(query, "lazy_multi_join_query")
  built <- sql_build(out, simulate_dbi())
  expect_true(built$distinct)
})

test_that("lazy_semi_join_query() checks arguments", {
  by0 <- list(
    x = "x",
    y = "x",
    x_as = ident("LHS"),
    y_as = ident("RHS"),
    na_matches = "never"
  )
  lmod <- purrr::list_modify

  my_lazy_semi_join_query <- function(
    x = lazy_frame(x = 1, y = 2)$lazy_query,
    y = lazy_frame(x = 1, z = 2)$lazy_query,
    anti = FALSE,
    by = by0
  ) {
    lazy_semi_join_query(
      x = x,
      y = y,
      vars = tibble(name = "x", var = "x"),
      anti = anti,
      by = by
    )
  }

  expect_snapshot(error = TRUE, {
    (my_lazy_semi_join_query(x = lazy_frame(x = 1)))
    (my_lazy_semi_join_query(y = lazy_frame(x = 1)))
  })

  expect_snapshot(error = TRUE, {
    (my_lazy_semi_join_query(by = lmod(by0, x = 1)))
  })

  expect_snapshot(error = TRUE, {
    (my_lazy_semi_join_query(anti = NA))
  })
})
