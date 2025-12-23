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

test_that("print method doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2, .name = "lf1")
  lf2 <- lazy_frame(x = 1, z = 2, .name = "lf2")
  lf3 <- lazy_frame(x = 1, z = 2, .name = "lf3")

  expect_snapshot(
    left_join(lf1, lf2, by = "x") |>
      left_join(lf3, by = "x") |>
      sql_build()
  )
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(inner_join(lf, lf))
  expect_snapshot(left_join(lf, lf))
  expect_snapshot(right_join(lf, lf))
  expect_snapshot(full_join(lf, lf))
})

test_that("only disambiguates shared variables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(left_join(lf1, lf2))
  expect_snapshot(left_join(lf1, lf2, by = c("y" = "z")))
})

test_that("disambiguate variables that only differ in case", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(X = 1, y = 2)
  expect_snapshot(left_join(lf1, lf2, by = "y"))
  expect_snapshot(full_join(lf1, lf2, by = "y"))
})

test_that("sql_on query doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 3)

  expect_snapshot(inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(left_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(right_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(full_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(semi_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(anti_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
})

test_that("sql_multi_join_vars generates expected SQL", {
  con <- simulate_dbi()

  # left_join(lf(x, a), lf(x, b), by = "x")
  expect_equal(
    sql_multi_join_vars(
      con,
      vars = tibble(
        name = c("x", "a", "b"),
        var = c("x", "a", "b"),
        table = c(1L, 1L, 2L)
      ),
      table_vars = list(left = c("x", "a"), right = c("x", "b")),
      use_star = TRUE,
      qualify_all_columns = FALSE
    ),
    sql('"left".*', b = '"b"')
  )
})
