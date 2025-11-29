test_that("filter captures local variables", {
  mf <- memdb_frame(x = 1:5, y = 5:1)

  z <- 3
  df1 <- mf |> filter(x > z) |> collect()
  df2 <- mf |> collect() |> filter(x > z)

  compare_tbl(df1, df2)
})

test_that("two filters equivalent to one", {
  mf <- memdb_frame(x = 1:5, y = 5:1)
  lf <- lazy_frame(x = 1:5, y = 5:1)

  df1 <- mf |> filter(x > 3) |> filter(y < 3)
  df2 <- mf |> filter(x > 3, y < 3)
  compare_tbl(df1, df2)

  lf1 <- lf |> filter(x > 3) |> filter(y < 3)
  lf2 <- lf |> filter(x > 3, y < 3)
  expect_equal(lf1 |> remote_query(), lf2 |> remote_query())
  expect_snapshot(lf1 |> remote_query())

  df1 <- mf |> filter(mean(x, na.rm = TRUE) > 3) |> filter(y < 3)
  df2 <- mf |> filter(mean(x, na.rm = TRUE) > 3, y < 3)
  compare_tbl(df1, df2)

  unique_column_name_reset()
  lf1 <- lf |> filter(mean(x, na.rm = TRUE) > 3) |> filter(y < 3)
  unique_column_name_reset()
  lf2 <- lf |> filter(mean(x, na.rm = TRUE) > 3, y < 3)
  expect_equal(lf1 |> remote_query(), lf2 |> remote_query())
  expect_snapshot(lf1 |> remote_query())
})


test_that("each argument gets implicit parens", {
  mf <- memdb_frame(
    v1 = c("a", "b", "a", "b"),
    v2 = c("b", "a", "a", "b"),
    v3 = c("a", "b", "c", "d")
  )

  mf1 <- mf |> filter((v1 == "a" | v2 == "a") & v3 == "a")
  mf2 <- mf |> filter(v1 == "a" | v2 == "a", v3 == "a")
  compare_tbl(mf1, mf2)
})

test_that("only add step if necessary", {
  lf <- lazy_frame(x = 1:3, y = 1:3)
  expect_equal(lf |> filter(), lf)
})

test_that("errors for named input", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_snapshot(error = TRUE, filter(lf, x = 1))
  expect_snapshot(error = TRUE, filter(lf, y > 1, x = 1))
})

test_that(".preserve is not supported", {
  lf <- lazy_frame(x = 1:3, y = 1:3)
  expect_snapshot(error = TRUE, lf |> filter(x == 1, .preserve = TRUE))
})

test_that("filter() inlined after select()", {
  lf <- lazy_frame(x = 1, y = 2)

  out <- lf |>
    select(y) |>
    filter(y > 1)

  expect_equal(
    remote_query(out),
    sql("SELECT `y`\nFROM `df`\nWHERE (`y` > 1.0)")
  )

  out <- lf |>
    select(z = x) |>
    filter(z == 1)
  lq <- out$lazy_query
  expect_equal(lq$select$expr, list(sym("x")))
  expect_equal(lq$where, list(quo(x == 1)), ignore_formula_env = TRUE)
})

test_that("filter() inlined after mutate()", {
  lf <- lazy_frame(x = 1, y = 2)

  out <- lf |>
    mutate(x = x + 1) |>
    filter(y == 1)
  lq <- out$lazy_query
  expect_equal(
    lq$select$expr,
    list(quo(x + 1), sym("y")),
    ignore_formula_env = TRUE
  )
  expect_equal(lq$where, list(quo(y == 1)), ignore_formula_env = TRUE)

  # can rename variable used in `filter()`
  out <- lf |>
    rename(z = x) |>
    filter(z == 1)
  lq <- out$lazy_query
  expect_equal(lq$select$expr, list(sym("x"), sym("y")))
  expect_equal(lq$select$name, c("z", "y"))
  expect_equal(lq$where, list(quo(x == 1)), ignore_formula_env = TRUE)

  # does not inline if uses mutated variable
  out2 <- lf |>
    mutate(x = x + 1) |>
    filter(x == 1)
  lq2 <- out2$lazy_query
  expect_equal(
    lq2$x$select$expr,
    list(quo(x + 1), sym("y")),
    ignore_formula_env = TRUE
  )
  expect_equal(lq2$select$expr, syms(c("x", "y")))
  expect_equal(lq2$where, list(quo(x == 1)), ignore_formula_env = TRUE)

  # does not inline if unclear whether uses mutated variable
  out3 <- lf |>
    mutate(x = x + 1) |>
    filter(y == sql("1"))
  lq3 <- out3$lazy_query
  expect_equal(lq3$select$expr, syms(c("x", "y")))
  expect_s3_class(lq3$x, "lazy_select_query")
  expect_equal(lq3$where, list(quo(y == sql("1"))), ignore_formula_env = TRUE)
})

test_that("filter isn't inlined after mutate with window function #1135", {
  lf <- lazy_frame(x = 1L, y = 1:2)
  out <- lf |>
    dplyr::mutate(z = sum(y, na.rm = TRUE)) |>
    dplyr::filter(y <= 1)

  lq <- out$lazy_query
  expect_equal(lq$select$expr, syms(c("x", "y", "z")))
  expect_equal(lq$where, list(quo(y <= 1)), ignore_formula_env = TRUE)
  expect_equal(
    quo_get_expr(lq$x$select$expr[[3]]),
    expr(sum(y, na.rm = TRUE))
  )

  out2 <- lf |>
    dplyr::mutate(z = sql("SUM(y) OVER ()")) |>
    dplyr::filter(y <= 1)

  lq2 <- out2$lazy_query
  expect_equal(lq2$select$expr, syms(c("x", "y", "z")))
  expect_equal(lq2$where, list(quo(y <= 1)), ignore_formula_env = TRUE)
  expect_equal(
    quo_get_expr(lq2$x$select$expr[[3]]),
    expr(sql("SUM(y) OVER ()"))
  )
})

# .by -------------------------------------------------------------------------

test_that("can group transiently using `.by`", {
  suppressWarnings(check_na_rm(FALSE))
  df <- memdb_frame(g = c(1, 1, 2, 1, 2), x = c(5, 10, 1, 2, 3))

  out <- filter(df, x > mean(x), .by = g) |>
    arrange(g, x) |>
    collect()

  expect_identical(out$g, c(1, 2))
  expect_identical(out$x, c(10, 3))
  expect_equal(group_vars(out), character())
})

test_that("catches `.by` with grouped-df", {
  df <- lazy_frame(x = 1)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    filter(gdf, .by = x)
  })
})

# SQL generation --------------------------------------------------------

test_that("filter calls windowed versions of sql functions", {
  df1 <- memdb_frame(x = 1:10, g = rep(c(1, 2), each = 5))

  out <- df1 |> group_by(g) |> filter(dplyr::row_number(x) < 3) |> collect()
  expect_equal(out$x, c(1L, 2L, 6L, 7L))
})

test_that("filter() can use window function and external vector - #1048", {
  to_filter <- 1:2
  expect_snapshot(
    lazy_frame(x = 1L) |>
      filter(x == max(x, na.rm = T), x %in% to_filter)
  )
})

test_that("recycled aggregates generate window function", {
  df1 <- memdb_frame(x = 1:10, g = rep(c(1, 2), each = 5))

  out <- df1 |>
    group_by(g) |>
    filter(x > mean(x, na.rm = TRUE)) |>
    collect()
  expect_equal(out$x, c(4L, 5L, 9L, 10L))
})

test_that("cumulative aggregates generate window function", {
  df1 <- memdb_frame(x = c(1:3, 2:4), g = rep(c(1, 2), each = 3))
  out <- df1 |>
    group_by(g) |>
    window_order(x) |>
    filter(cumsum(x) > 3)

  expect_equal(pull(out, x), c(3L, 3L, 4L))
})

test_that("filter() after summarise() uses `HAVING`", {
  lf <- lazy_frame(g = 1, h = 1, x = 1) |>
    group_by(g, h) |>
    summarise(x_mean = mean(x, na.rm = TRUE), .groups = "drop_last")
  mf <- memdb_frame(g = c(1, 1, 1, 2, 2), h = 1, x = 1:5) |>
    group_by(g, h) |>
    summarise(x_mean = mean(x, na.rm = TRUE), .groups = "drop_last")

  # use `HAVING`
  expect_snapshot((out <- lf |> filter(g == 1)))
  expect_equal(
    out$lazy_query$having,
    list(quo(g == 1)),
    ignore_formula_env = TRUE
  )
  expect_equal(
    out$lazy_query$group_by,
    list(sym("g"), sym("h")),
    ignore_formula_env = TRUE
  )
  expect_equal(op_grps(out), "g")

  expect_equal(
    mf |>
      filter(g == 1) |>
      collect(),
    tibble(g = 1, h = 1, x_mean = 2) |> group_by(g)
  )

  # Can use freshly aggregated column
  expect_snapshot((out <- lf |> filter(x_mean > 1)))
  expect_equal(
    out$lazy_query$having,
    list(quo(mean(x, na.rm = TRUE) > 1)),
    ignore_formula_env = TRUE
  )

  expect_equal(
    mf |>
      filter(x_mean > 3) |>
      collect(),
    tibble(g = 2, h = 1, x_mean = 4.5) |> group_by(g)
  )

  # multiple `filter()` combine instead of overwrite
  expect_snapshot(
    (out <- lf |>
      filter(g == 1) |>
      filter(g == 2))
  )
  expect_equal(
    out$lazy_query$having,
    list(quo(g == 1), quo(g == 2)),
    ignore_formula_env = TRUE
  )

  expect_snapshot(
    (out <- lf |>
      filter(g == 1) |>
      filter(h == 2))
  )
  expect_equal(
    out$lazy_query$having,
    list(quo(g == 1), quo(h == 2)),
    ignore_formula_env = TRUE
  )

  # `window_order()` and `window_frame()` do not matter
  out <- lazy_frame(g = 1, h = 1, x = 1) |>
    window_order(h) |>
    window_frame(-3) |>
    group_by(g, h) |>
    summarise(x_mean = mean(x, na.rm = TRUE), .groups = "drop_last") |>
    filter(x_mean > 1)

  lq <- out$lazy_query
  expect_equal(
    lq$having,
    list(quo(mean(x, na.rm = TRUE) > 1)),
    ignore_formula_env = TRUE
  )
  # TODO should the `order_vars` and the `frame` really survive `summarise()`?
  expect_equal(lq$order_vars, list(expr(h)))
  expect_equal(lq$frame, list(range = c(-3, Inf)))
})

test_that("`HAVING` supports expressions #1128", {
  lf <- lazy_frame(x = 1)

  expect_snapshot({
    lf |>
      summarise(x_sum = sum(x, na.rm = TRUE)) |>
      filter(!is.na(x_sum))
  })

  out <- lf |>
    summarise(x_sum = sum(x, na.rm = TRUE)) |>
    filter(!is.na(x_sum))
  expect_equal(
    out$lazy_query$having,
    list(quo(!is.na(sum(x, na.rm = TRUE)))),
    ignore_formula_env = TRUE
  )

  # correctly handles environments
  y <- 1L
  f <- function(lf, y = 2L) {
    lf |> summarise(x_sum = sum(x, na.rm = TRUE) - y)
  }

  out <- f(lf) |>
    filter(!is.na(x_sum + y))

  expect_equal(
    out$lazy_query$having,
    list(quo(!is.na(sum(x, na.rm = TRUE) - 2L + 1L))),
    ignore_formula_env = TRUE
  )
})

test_that("filter() after mutate() does not use `HAVING`", {
  lf <- lazy_frame(g = 1, h = 1, x = 1) |>
    group_by(g, h) |>
    mutate(x_mean = mean(x, na.rm = TRUE))

  expect_snapshot((out <- lf |> filter(x_mean > 1)))
  lq <- out$lazy_query
  expect_s3_class(lq$x, "lazy_select_query")
})

test_that("filter() using a window function after summarise() does not use `HAVING`", {
  lf <- lazy_frame(g = 1, h = 1, x = 1) |>
    group_by(g, h) |>
    summarise(x_mean = mean(x, na.rm = TRUE), .groups = "drop_last")

  expect_snapshot((out <- lf |> filter(cumsum(x_mean) == 1)))
  lq <- out$lazy_query
  expect_s3_class(lq$x, "lazy_select_query")
})

# sql_build ---------------------------------------------------------------

test_that("filter generates simple expressions", {
  out <- lazy_frame(x = 1) |>
    filter(x > 1L) |>
    sql_build()

  expect_equal(out$where, sql('`x` > 1'))
})


# lazy_select_query -------------------------------------------------------

test_that("generates correct lazy_select_query", {
  lf <- lazy_frame(x = 1:3, y = 3:1)

  expect_equal(
    filter(lf, x > 1)$lazy_query,
    lazy_select_query(
      x = lf$lazy_query,
      select = syms(set_names(colnames(lf))),
      where = list(quo(x > 1))
    ),
    ignore_formula_env = TRUE
  )

  out <- lf |>
    filter(mean(x, na.rm = TRUE) > 1)

  expect_equal(
    out$lazy_query,
    lazy_select_query(
      x = out$lazy_query$x,
      select = syms(set_names(colnames(lf))),
      where = list(expr(col01 > 1))
    ),
    ignore_formula_env = TRUE
  )

  expect_equal(
    out$lazy_query$x,
    lazy_select_query(
      x = lf$lazy_query,
      select_operation = "mutate",
      select = list(
        x = sym("x"),
        y = sym("y"),
        col01 = quo(mean(x, na.rm = TRUE))
      )
    ),
    ignore_formula_env = TRUE
  )
})
