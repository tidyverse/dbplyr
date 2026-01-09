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

test_that("sql_build.lazy_multi_join_query() includes where", {
  lf1 <- lazy_frame(x = 1, y = 1)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- lf1 |>
    left_join(lf2, by = "x") |>
    filter(y > 1, z < 5)

  query <- out$lazy_query
  expect_s3_class(query, "lazy_multi_join_query")
  expect_length(query$where, 2)

  built <- sql_build(out, simulate_dbi())
  expect_length(built$where, 2)
})

test_that("multi_join where clause uses qualified column names", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 3)

  expect_snapshot(left_join(lf1, lf2, by = "x") |> filter(y > 1))
  expect_snapshot(left_join(lf1, lf2, by = "x") |> filter(z > 1))
  expect_snapshot(left_join(lf1, lf2, by = "x") |> filter(y > 1, z < 5))

  lf3 <- lazy_frame(x = 1, y = 3)
  expect_snapshot(left_join(lf1, lf3, by = "x") |> filter(y.x > 1))
})

test_that("where clause is updated when vars are renamed by later join (#1770)", {
  lf1 <- lazy_frame(x = 1, y = 2, .name = "t1")
  lf2 <- lazy_frame(x = 1, z = 3, .name = "t2")
  lf3 <- lazy_frame(x = 1, z = 4, .name = "t3")

  # When z gets renamed to z.x, the filter should use the new name
  out <- lf1 |>
    left_join(lf2, by = "x") |>
    filter(z == 1) |>
    left_join(lf3, by = "x")

  expect_equal(out$lazy_query$where, list(expr(z.x == 1)))
})

test_that("generated sql doesn't change unexpectedly", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(inner_join(lf, lf))
  expect_snapshot(left_join(lf, lf))
})

test_that("only disambiguates shared variables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)
  expect_snapshot(left_join(lf1, lf2, by = c("x" = "x")))
  expect_snapshot(left_join(lf1, lf2, by = c("y" = "z")))
})

test_that("disambiguate variables that only differ in case", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(X = 1, y = 2)
  expect_snapshot(left_join(lf1, lf2, by = "y"))
})

test_that("sql_on query doesn't change unexpectedly", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 3)

  expect_snapshot(inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
  expect_snapshot(left_join(lf1, lf2, sql_on = "LHS.y < RHS.z"))
})

test_that("sql_multi_join_select generates expected SQL", {
  con <- simulate_dbi()

  vars <- tibble(
    name = c("x", "a", "b"),
    var = c("x", "a", "b"),
    table = c(1L, 1L, 2L)
  )
  # left_join(lf(x, a), lf(x, b), by = "x")
  expect_equal(
    sql_multi_join_select(
      con,
      vars = vars,
      table_vars = list(left = c("x", "a"), right = c("x", "b"))
    ),
    sql('"left".*', '"b"')
  )

  # Left join with * - uses star for first table when all vars selected in order
  vars <- tibble(
    name = c("y", "a", "b", "c"),
    table = c(1L, 1L, 1L, 2L),
    var = c("y", "a", "b", "c")
  )
  expect_equal(
    sql_multi_join_select(
      con,
      vars = vars,
      table_vars = list(one = c("y", "a", "b"), two = c("y", "c"))
    ),
    sql('"one".*', '"c"')
  )

  # Left join with duplicated names - needs qualified column references
  vars <- tibble(
    name = c("y", "a.y", "a.x"),
    table = c(1L, 2L, 1L),
    var = c("x", "a", "a")
  )
  expect_equal(
    sql_multi_join_select(
      con,
      vars = vars,
      table_vars = list(one = c("x", "a"), two = c("x", "a"))
    ),
    sql('"one"."x" AS "y"', '"two"."a" AS "a.y"', '"one"."a" AS "a.x"')
  )
})
