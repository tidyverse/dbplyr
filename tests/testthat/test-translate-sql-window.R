test_that("window functions without group have empty over", {
  con <- simulate_dbi()
  expect_translation(con, n(), "COUNT(*) OVER ()")
  expect_translation(con, sum(x, na.rm = TRUE), "SUM(`x`) OVER ()")
})

test_that("aggregating window functions ignore order_by", {
  con <- simulate_dbi()
  expect_translation(con, n(), "COUNT(*) OVER ()", vars_order = "x")
  expect_translation(
    con,
    sum(x, na.rm = TRUE),
    "SUM(`x`) OVER ()",
    vars_order = "x"
  )
})

test_that("count uses order_by if frame is used", {
  con <- simulate_dbi()
  expect_translation(
    con,
    n(),
    "COUNT(*) OVER (ORDER BY `x` ROWS BETWEEN 2 PRECEDING AND 1 FOLLOWING)",
    vars_order = "x",
    vars_frame = c(-2, 1)
  )
})

test_that("order_by overrides default ordering", {
  con <- simulate_dbi()
  expect_translation(
    con,
    order_by(y, cumsum(x)),
    "SUM(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)",
    vars_order = "x"
  )
  expect_translation(
    con,
    order_by(y, cummean(x)),
    "AVG(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)",
    vars_order = "x"
  )
  expect_translation(
    con,
    order_by(y, cummin(x)),
    "MIN(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)",
    vars_order = "x"
  )
  expect_translation(
    con,
    order_by(y, cummax(x)),
    "MAX(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)",
    vars_order = "x"
  )
})

test_that("cumulative windows warn if no order", {
  con <- simulate_dbi()
  expect_warning(
    translate_sql(cumsum(x), con = con),
    "does not have explicit order"
  )
  expect_warning(translate_sql(cumsum(x), con = con, vars_order = "x"), NA)
})

test_that("ntile always casts to integer", {
  con <- simulate_dbi()
  expect_translation(con, ntile(x, 10.5), "NTILE(10) OVER (ORDER BY `x`)")
})

test_that("first, last, and nth translated to _value", {
  con <- simulate_dbi()
  expect_translation(con, first(x), "FIRST_VALUE(`x`) OVER ()")
  expect_translation(
    con,
    first(x, na_rm = TRUE),
    "FIRST_VALUE(`x` IGNORE NULLS) OVER ()"
  )
  # `last()` must default to unbounded preceding and following
  expect_translation(
    con,
    last(x),
    "LAST_VALUE(`x`) OVER (ORDER BY `a` ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)",
    vars_order = "a"
  )
  expect_translation(
    con,
    last(x),
    "LAST_VALUE(`x`) OVER (ORDER BY `a` ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)",
    vars_order = "a",
    vars_frame = c(0, Inf)
  )
  expect_translation(
    con,
    nth(x, 3),
    "NTH_VALUE(`x`, 3) OVER (ORDER BY `a` ROWS UNBOUNDED PRECEDING)",
    vars_order = "a",
    vars_frame = c(-Inf, 0)
  )

  # can also use a column #1236
  expect_translation(
    con,
    nth(x, n),
    "NTH_VALUE(`x`, `n`) OVER (ORDER BY `a` ROWS UNBOUNDED PRECEDING)",
    vars_order = "a",
    vars_frame = c(-Inf, 0)
  )
})

test_that("can override frame of recycled functions", {
  con <- simulate_dbi()
  expect_translation(
    con,
    sum(x, na.rm = TRUE),
    "SUM(`x`) OVER (ORDER BY `y` ROWS 1 PRECEDING)",
    vars_frame = c(-1, 0),
    vars_order = "y"
  )
})

test_that("frame is checked", {
  con <- simulate_dbi()
  expect_snapshot(
    error = TRUE,
    translate_sql(sum(x, na.rm = TRUE), con = con, vars_frame = c(1, 0))
  )
})

test_that("win_rank works", {
  con <- simulate_dbi()
  sql_row_number <- win_rank("ROW_NUMBER")
  expect_translation(
    con,
    row_number(x),
    "CASE
WHEN (NOT((`x` IS NULL))) THEN ROW_NUMBER() OVER (PARTITION BY (CASE WHEN ((`x` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `x`)
END"
  )
})

test_that("win_rank(desc(x)) works", {
  con <- simulate_dbi()
  expect_translation(
    con,
    row_number(desc(x)),
    "CASE
WHEN (NOT((`x` IS NULL))) THEN ROW_NUMBER() OVER (PARTITION BY (CASE WHEN ((`x` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `x` DESC)
END"
  )
})

test_that("win_rank(tibble()) works", {
  con <- simulate_dbi()

  expect_equal(
    translate_sql(row_number(tibble(x)), con = con),
    translate_sql(row_number(x), con = con)
  )
  expect_equal(
    translate_sql(row_number(tibble(desc(x))), con = con),
    translate_sql(row_number(desc(x)), con = con)
  )

  expect_translation(
    con,
    row_number(tibble(x, desc(y))),
    "CASE
WHEN (NOT((`x` IS NULL)) AND NOT((`y` IS NULL))) THEN ROW_NUMBER() OVER (PARTITION BY (CASE WHEN ((`x` IS NULL) OR (`y` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `x`, `y` DESC)
END"
  )
})

test_that("win_rank(c()) gives an informative error", {
  con <- simulate_dbi()
  expect_snapshot(error = TRUE, {
    translate_sql(row_number(c(x)), con = con)
  })
})

test_that("row_number() with and without group_by() and arrange()", {
  mf <- lazy_frame(x = c(1:5), y = c(rep("A", 5)), con = simulate_dbi())
  expect_snapshot(mf |> mutate(rown = row_number()))
  expect_snapshot(mf |> group_by(y) |> mutate(rown = row_number()))
  expect_snapshot(
    mf |> group_by(y) |> arrange(y) |> mutate(rown = row_number())
  )
  expect_snapshot(mf |> arrange(y) |> mutate(rown = row_number()))
})

test_that("win_cumulative works", {
  con <- simulate_dbi()
  expect_translation(
    con,
    cumsum(x, "y"),
    "SUM(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)"
  )

  # NA values results in NA rank
  db <- memdb_frame(x = c(1, 2, NA, 3))
  expect_equal(
    db |> mutate(rank = dense_rank(x)) |> collect() |> arrange(x),
    tibble(x = c(1:3, NA), rank = c(1:3, NA))
  )
})


# win_over ----------------------------------------------------------------

test_that("over() only requires first argument", {
  con <- simulate_dbi()
  expect_equal(win_over("X", con = con), sql("'X' OVER ()"))
})

test_that("multiple group by or order values don't have parens", {
  con <- simulate_dbi()

  expect_equal(
    win_over(ident("x"), order = c("x", "y"), con = con),
    sql("`x` OVER (ORDER BY `x`, `y`)")
  )
  expect_equal(
    win_over(ident("x"), partition = c("x", "y"), con = con),
    sql("`x` OVER (PARTITION BY `x`, `y`)")
  )
})

# window_frame ------------------------------------------------------------

test_that("window_frame()", {
  lf <- lazy_frame(x = runif(10), y = 1:10)

  expect_snapshot(
    lf |>
      window_frame(-3, 0) |>
      window_order(x) |>
      mutate(z = sum(y, na.rm = TRUE)) |>
      show_query()
  )

  expect_snapshot(
    lf |>
      window_frame(-3) |>
      window_order(x) |>
      mutate(z = sum(y, na.rm = TRUE)) |>
      show_query()
  )
})

test_that("window_frame() checks arguments", {
  skip_if(getRversion() <= '3.5.0', "R too old")
  lf <- lazy_frame(x = runif(10), y = 1:10)

  expect_snapshot(error = TRUE, window_frame(lf, "a"))
  expect_snapshot(error = TRUE, window_frame(lf, 1:2))

  expect_snapshot(error = TRUE, window_frame(lf, 1, "a"))
  expect_snapshot(error = TRUE, window_frame(lf, 1, 1:2))
})


# named windows -----------------------------------------------------------

test_that("names windows automatically", {
  lf <- lazy_frame(
    col1 = runif(3),
    col2 = runif(3),
    col3 = runif(3),
    col4 = runif(3),
    part = c("a", "a", "b"),
    ord = 3:1,
    con = simulate_sqlite()
  ) |>
    group_by(part) |>
    window_order(ord)

  lf1 <- lf |>
    transmute(
      across(c(col1, col2), ~ sum(.x, na.rm = TRUE)),
      across(c(col3, col4), ~ order_by(desc(ord), cumsum(.x)))
    )

  sql_list <- get_select_sql(
    select = lf1$lazy_query$select,
    select_operation = "mutate",
    in_vars = op_vars(lf),
    table_alias = "df",
    con = simulate_sqlite(),
    use_star = TRUE
  )
  expect_equal(
    sql_list$window_sql,
    sql(
      "`win1` AS (PARTITION BY `part`)",
      "`win2` AS (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING)"
    )
  )
  expect_equal(
    sql_list$select_sql,
    sql(
      part = "`part`",
      col1 = "SUM(`col1`) OVER `win1`",
      col2 = "SUM(`col2`) OVER `win1`",
      col3 = "SUM(`col3`) OVER `win2`",
      col4 = "SUM(`col4`) OVER `win2`"
    )
  )

  # Different order does not confuse naming of windows
  lf2 <- lf |>
    transmute(
      col1 = sum(col1, na.rm = TRUE),
      col3 = order_by(desc(ord), cumsum(col3)),
      col2 = sum(col2, na.rm = TRUE),
      col4 = order_by(desc(ord), cumsum(col4))
    )

  sql_list <- get_select_sql(
    select = lf2$lazy_query$select,
    select_operation = "mutate",
    in_vars = op_vars(lf),
    table_alias = "df",
    con = simulate_sqlite(),
    use_star = TRUE
  )
  expect_equal(
    sql_list$window_sql,
    sql(
      "`win1` AS (PARTITION BY `part`)",
      "`win2` AS (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING)"
    )
  )
  expect_equal(
    sql_list$select_sql,
    sql(
      part = "`part`",
      col1 = "SUM(`col1`) OVER `win1`",
      col3 = "SUM(`col3`) OVER `win2`",
      col2 = "SUM(`col2`) OVER `win1`",
      col4 = "SUM(`col4`) OVER `win2`"
    )
  )
})

test_that("only name windows if they appear multiple times", {
  lf <- lazy_frame(
    col1 = runif(3),
    col2 = runif(3),
    col3 = runif(3),
    part = c("a", "a", "b"),
    ord = 3:1,
    con = simulate_sqlite()
  ) |>
    group_by(part) |>
    window_order(ord) |>
    transmute(
      across(c(col1, col2), ~ sum(.x, na.rm = TRUE)),
      across(c(col3), ~ order_by(desc(ord), cumsum(.x)))
    )

  sql_list <- get_select_sql(
    select = lf$lazy_query$select,
    select_operation = "mutate",
    in_vars = op_vars(lf),
    table_alias = "df",
    con = simulate_sqlite(),
    use_star = TRUE
  )
  expect_equal(sql_list$window_sql, sql("`win1` AS (PARTITION BY `part`)"))
  expect_equal(
    sql_list$select_sql,
    sql(
      part = "`part`",
      col1 = "SUM(`col1`) OVER `win1`",
      col2 = "SUM(`col2`) OVER `win1`",
      col3 = "SUM(`col3`) OVER (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING)"
    )
  )
})

test_that("name windows only if supported", {
  lf <- lazy_frame(
    col1 = runif(3),
    col2 = runif(3),
    part = c("a", "a", "b"),
    con = simulate_hana()
  ) |>
    group_by(part) |>
    transmute(
      across(c(col1, col2), ~ sum(.x, na.rm = TRUE))
    )

  sql_list <- get_select_sql(
    select = lf$lazy_query$select,
    select_operation = "mutate",
    in_vars = op_vars(lf),
    table_alias = "df",
    con = simulate_hana(),
    use_star = TRUE
  )
  expect_equal(sql_list$window_sql, character())
  expect_equal(
    sql_list$select_sql,
    sql(
      part = "`part`",
      col1 = "SUM(`col1`) OVER (PARTITION BY `part`)",
      col2 = "SUM(`col2`) OVER (PARTITION BY `part`)"
    )
  )
})
