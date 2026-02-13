test_that("basic distinct behaviour is as expected", {
  db <- local_memdb_frame(x = c(1, 1, 1, 1), y = c(1, 1, 2, 2))

  expect_equal(db |> distinct() |> collect(), tibble(x = c(1, 1), y = c(1, 2)))
  expect_equal(db |> distinct(x) |> collect(), tibble(x = 1))
  expect_equal(db |> distinct(y) |> collect(), tibble(y = c(1, 2)))
})

test_that("correctly inlines across all verbs", {
  lf <- lazy_frame(x = 1, y = 2)

  # single table verbs
  expect_selects(lf |> arrange(x) |> distinct(), 1)
  expect_selects(lf |> distinct() |> distinct(), 1)
  expect_selects(lf |> filter(x == 1) |> distinct(), 1)
  expect_selects(lf |> head(1) |> distinct(), 2)
  expect_selects(lf |> mutate(z = x + 1) |> distinct(), 1)
  expect_selects(lf |> select(y = x) |> distinct(), 1)
  expect_selects(lf |> summarise(y = mean(x)) |> distinct(), 1)

  # two table verbs
  lf2 <- lazy_frame(x = 1)
  expect_selects(lf |> left_join(lf2, by = "x") |> distinct(), 1)
  expect_selects(lf |> right_join(lf2, by = "x") |> distinct(), 2)
  expect_selects(lf |> semi_join(lf2, by = "x") |> distinct(), 3)
  expect_selects(lf |> union(lf2) |> distinct(), 3)

  # special cases
  # doesn't inline if .keep_all is TRUE, since that requires a window function
  expect_selects(
    lf |> left_join(lf2, by = "x") |> distinct(y, .keep_all = TRUE),
    3
  )
})

test_that("distinct returns all columns when .keep_all is TRUE", {
  mf <- local_memdb_frame(x = c(1, 1, 2, 2), y = 1:4)

  result <- mf |> distinct(x, .keep_all = TRUE) |> collect()
  expect_named(result, c("x", "y"))
  expect_equal(result$x, c(1, 2))
  expect_equal(group_vars(result), character())
})

test_that("distinct doesn't duplicate column names if grouped (#354)", {
  df <- lazy_frame(a = 1)
  expect_equal(df |> group_by(a) |> distinct() |> op_vars(), "a")
})

test_that("distinct respects groups", {
  df <- local_memdb_frame(a = 1:2, b = 1) |> group_by(a)
  expect_equal(df |> group_by(a) |> distinct() |> op_vars(), c("a", "b"))
})


test_that("distinct respects groups when .keep_all is TRUE", {
  mf <- local_memdb_frame(x = c(1, 1, 2, 2), y = 1:4)

  result <- mf |> group_by(x) |> distinct(.keep_all = TRUE) |> collect()
  expect_named(result, c("x", "y"))
  expect_equal(result$x, c(1, 2))
  expect_equal(group_vars(result), "x")
})

test_that("distinct can select variables via pick() #1125", {
  lf <- lazy_frame(x_1 = 1, x_2 = 1, y = 1)
  expect_equal(
    lf |> distinct(pick(starts_with("x_"))) |> remote_query(),
    sql("SELECT DISTINCT \"x_1\", \"x_2\"\nFROM \"df\"")
  )
})

test_that("distinct() produces optimized SQL", {
  lf <- lazy_frame(x = 1, y = 1)

  # can use renamed variable
  out <- lf |>
    select(a = x, y) |>
    distinct(a)

  expect_equal(
    remote_query(out),
    sql("SELECT DISTINCT \"x\" AS \"a\"\nFROM \"df\"")
  )

  expect_true(out$lazy_query$distinct)
  expect_equal(out$lazy_query$select$name, "a")
  expect_equal(out$lazy_query$select$expr, syms("x"))

  # unnecessary extra variables do not matter
  out <- lf |>
    mutate(z = 1) |>
    group_by(x) |>
    distinct(y)
  expect_s3_class(out$lazy_query$x, "lazy_base_local_query")
  expect_equal(out$lazy_query$select$name, c("x", "y"))
  expect_equal(out$lazy_query$select$expr, syms(c("x", "y")))

  # inlined after `summarise()`
  out <- lf |>
    group_by(x) |>
    summarise(y = mean(y, na.rm = TRUE)) |>
    distinct(x, y)

  expect_equal(
    remote_query(out),
    sql(
      "SELECT DISTINCT \"x\", AVG(\"y\") AS \"y\"\nFROM \"df\"\nGROUP BY \"x\""
    )
  )

  expect_true(out$lazy_query$distinct)
  expect_equal(out$lazy_query$select$name, c("x", "y"))
  expect_equal(
    out$lazy_query$select$expr,
    list(sym("x"), expr(mean(y, na.rm = TRUE)))
  )
  expect_equal(out$lazy_query$group_by, syms("x"))

  # inlined after `filter()`
  out <- lf |>
    filter(x == 1L) |>
    distinct(y)

  expect_equal(
    remote_query(out),
    sql("SELECT DISTINCT \"y\"\nFROM \"df\"\nWHERE (\"x\" = 1)")
  )

  expect_true(out$lazy_query$distinct)
  expect_equal(out$lazy_query$select$name, "y")
  expect_equal(out$lazy_query$select$expr, syms("y"))
  expect_equal(out$lazy_query$where, list(expr(x == 1L)))

  # Note: currently this needs `distinct()` or `distinct(x, y)` because
  # `summarise()` + `select()` is not inlined.
  out <- lf |>
    group_by(x) |>
    summarise(y = mean(y, na.rm = TRUE)) |>
    filter(x == 1) |>
    distinct(x, y)

  expect_equal(
    remote_query(out),
    sql(
      "SELECT DISTINCT \"x\", AVG(\"y\") AS \"y\"\nFROM \"df\"\nGROUP BY \"x\"\nHAVING (\"x\" = 1.0)"
    )
  )
  expect_true(out$lazy_query$distinct)
  expect_equal(out$lazy_query$select$name, c("x", "y"))
  expect_equal(
    out$lazy_query$select$expr,
    list(expr(x), expr(mean(y, na.rm = TRUE)))
  )

  out <- lf |>
    arrange(y) |>
    distinct(x)

  expect_equal(
    remote_query(out),
    sql("SELECT DISTINCT \"x\"\nFROM \"df\"\nORDER BY \"y\"")
  )

  expect_snapshot(
    (out <- lf |>
      head(2) |>
      distinct(x, y))
  )

  expect_s3_class(out$lazy_query$x, "lazy_select_query")
  expect_equal(out$lazy_query$x$limit, 2)
})

test_that("distinct() after join is inlined", {
  lf1 <- lazy_frame(x = 1, y = 1)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- lf1 |>
    left_join(lf2, by = "x") |>
    distinct()
  expect_s3_class(out$lazy_query, "lazy_multi_join_query")
  expect_snapshot(show_query(out))

  out <- lf1 |>
    left_join(lf2, by = "x") |>
    distinct(x)
  expect_s3_class(out$lazy_query, "lazy_multi_join_query")
  expect_snapshot(show_query(out))
})

# sql-render --------------------------------------------------------------

test_that("distinct adds DISTINCT suffix", {
  out <- local_memdb_frame(x = c(1, 1)) |> distinct()

  expect_match(out |> sql_render(), "SELECT DISTINCT")
  expect_equal(out |> collect(), tibble(x = 1))
})

test_that("distinct can compute variables", {
  out <- local_memdb_frame(x = c(2, 1), y = c(1, 2)) |>
    distinct(z = x + y)
  expect_equal(out |> collect(), tibble(z = 3))
})

test_that("distinct ignores groups when computing variables (#1081)", {
  df <- tibble(g = c("a", "a", "b"), x = c(1, 2, 3))

  db <- local_memdb_frame("df", g = c("a", "a", "b"), x = c(1, 2, 3))
  db_distinct <- db |> group_by(g) |> distinct(n = n()) |> ungroup()

  expect_snapshot(show_query(db_distinct))
  expect_equal(collect(db_distinct), tibble(g = c("a", "b"), n = 3))
})

test_that("distinct can compute variables when .keep_all is TRUE", {
  out <- local_memdb_frame(x = c(2, 1), y = c(1, 2)) |>
    distinct(z = x + y, .keep_all = TRUE) |>
    collect()

  expect_named(out, c("x", "y", "z"))
  expect_equal(out$z, 3)
})

test_that("distinct respects window_order when .keep_all is TRUE", {
  mf <- local_memdb_frame(x = c(1, 1, 2, 2), y = 1:4)
  out <- mf |>
    window_order(desc(y)) |>
    distinct(x, .keep_all = TRUE)

  expect_equal(out |> collect(), tibble(x = 1:2, y = c(2, 4)))

  lf <- lazy_frame(x = c(1, 1, 2, 2), y = 1:4)
  expect_snapshot(
    lf |>
      window_order(desc(y)) |>
      distinct(x, .keep_all = TRUE)
  )
})

test_that("distinct uses dummy window order when .keep_all is TRUE and no order is used", {
  lf <- lazy_frame(x = 1, y = 2)
  expect_snapshot(lf |> distinct(x, .keep_all = TRUE))
})

# sql_build ---------------------------------------------------------------

test_that("distinct sets flagged", {
  out1 <- lazy_frame(x = 1) |>
    select() |>
    sql_build()
  expect_false(out1$distinct)

  out2 <- lazy_frame(x = 1) |>
    distinct() |>
    sql_build()
  expect_true(out2$distinct)
})

# ops ---------------------------------------------------------------------

test_that("distinct produces correct vars", {
  out <- lazy_frame(x = 1, y = 2) |> distinct()
  expect_equal(op_vars(out), c("x", "y"))

  out <- lazy_frame(x = 1, y = 2, z = 3) |> distinct(x, y)
  expect_equal(op_vars(out), c("x", "y"))

  out <- lazy_frame(x = 1, y = 2, z = 3) |> distinct(a = x, b = y)
  expect_equal(op_vars(out), c("a", "b"))

  out <- lazy_frame(x = 1, y = 2, z = 3) |> group_by(x) |> distinct(y)
  expect_equal(op_vars(out), c("x", "y"))
})

test_that("distinct produces correct vars when .keep_all is TRUE", {
  lf <- lazy_frame(x = 1, y = 2)
  out <- lf |> distinct(.keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y"))

  out <- lf |> distinct(x, .keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y"))

  out <- lf |> distinct(a = x, .keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y", "a"))

  out <- lazy_frame(x = 1, y = 2, z = 3) |>
    group_by(x) |>
    distinct(y, .keep_all = TRUE)
  expect_equal(op_vars(out), c("x", "y", "z"))
})

test_that("distinct respects order of the specified variables (#3195, #6156)", {
  d <- lazy_frame(x = 1:2, y = 3:4)
  expect_equal(colnames(distinct(d, y, x)), c("y", "x"))
})

test_that("distinct adds grouping variables to front if missing", {
  d <- lazy_frame(x = 1:2, y = 3:4)
  expect_equal(colnames(distinct(group_by(d, y), x)), c("y", "x"))
  expect_equal(colnames(distinct(group_by(d, y), x, y)), c("x", "y"))
})
