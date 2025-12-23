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
