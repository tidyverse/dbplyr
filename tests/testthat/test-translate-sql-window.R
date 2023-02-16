test_that("window functions without group have empty over", {
  expect_equal(translate_sql(n()), sql("COUNT(*) OVER ()"))
  expect_equal(translate_sql(sum(x, na.rm = TRUE)), sql("SUM(`x`) OVER ()"))
})

test_that("aggregating window functions ignore order_by", {
  expect_equal(
    translate_sql(n(), vars_order = "x"),
    sql("COUNT(*) OVER ()")
  )
  expect_equal(
    translate_sql(sum(x, na.rm = TRUE), vars_order = "x"),
    sql("SUM(`x`) OVER ()")
  )
})

test_that("count uses order_by if frame is used", {
  expect_equal(
    translate_sql(n(), vars_order = "x", vars_frame = c(-2, 1)),
    sql("COUNT(*) OVER (ORDER BY `x` ROWS BETWEEN 2 PRECEDING AND 1 FOLLOWING)")
  )
})

test_that("order_by overrides default ordering", {
  expect_equal(
    translate_sql(order_by(y, cumsum(x)), vars_order = "x"),
    sql("SUM(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)")
  )
  expect_equal(
    translate_sql(order_by(y, cummean(x)), vars_order = "x"),
    sql("AVG(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)")
  )
  expect_equal(
    translate_sql(order_by(y, cummin(x)), vars_order = "x"),
    sql("MIN(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)")
  )
  expect_equal(
    translate_sql(order_by(y, cummax(x)), vars_order = "x"),
    sql("MAX(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)")
  )
})

test_that("cumulative windows warn if no order", {
  expect_warning(translate_sql(cumsum(x)), "does not have explicit order")
  expect_warning(translate_sql(cumsum(x), vars_order = "x"), NA)
})

test_that("ntile always casts to integer", {
  expect_equal(
    translate_sql(ntile(x, 10.5)),
    sql("NTILE(10) OVER (ORDER BY `x`)")
  )
})

test_that("first, last, and nth translated to _value", {
  expect_equal(
    translate_sql(first(x)),
    sql("FIRST_VALUE(`x`) OVER ()")
  )
  # `last()` must default to unbounded preceding and following
  expect_equal(
    translate_sql(last(x), vars_order = "a"),
    sql("LAST_VALUE(`x`) OVER (ORDER BY `a` ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)")
  )
  expect_equal(
    translate_sql(last(x), vars_order = "a", vars_frame = c(0, Inf)),
    sql("LAST_VALUE(`x`) OVER (ORDER BY `a` ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)")
  )
  expect_equal(
    translate_sql(nth(x, 3), vars_order = "a", vars_frame = c(-Inf, 0)),
    sql("NTH_VALUE(`x`, 3) OVER (ORDER BY `a` ROWS UNBOUNDED PRECEDING)")
  )
})

test_that("can override frame of recycled functions", {
  expect_equal(
    translate_sql(sum(x, na.rm = TRUE), vars_frame = c(-1, 0), vars_order = "y"),
    sql("SUM(`x`) OVER (ORDER BY `y` ROWS 1 PRECEDING)")
  )
})

test_that("frame is checked", {
  expect_snapshot(
    error = TRUE,
    translate_sql(sum(x, na.rm = TRUE), vars_frame = c(1, 0))
  )
})

test_that("win_rank works", {
  local_con(simulate_dbi())
  sql_row_number <- win_rank("ROW_NUMBER")
  expect_equal(
    translate_sql(row_number(x)),
    sql("CASE
WHEN (NOT((`x` IS NULL))) THEN ROW_NUMBER() OVER (PARTITION BY (CASE WHEN ((`x` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `x`)
END")
  )
})

test_that("win_rank(desc(x)) works", {
  local_con(simulate_dbi())
  expect_equal(
    translate_sql(row_number(desc(x))),
    sql("CASE
WHEN (NOT((`x` IS NULL))) THEN ROW_NUMBER() OVER (PARTITION BY (CASE WHEN ((`x` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `x` DESC)
END")
  )
})

test_that("win_rank(tibble()) works", {
  local_con(simulate_dbi())

  expect_equal(
    translate_sql(row_number(tibble(x))),
    translate_sql(row_number(x))
  )
  expect_equal(
    translate_sql(row_number(tibble(desc(x)))),
    translate_sql(row_number(desc(x)))
  )

  expect_equal(
    translate_sql(row_number(tibble(x, desc(y)))),
    sql("CASE
WHEN (NOT((`x` IS NULL)) AND NOT((`y` IS NULL))) THEN ROW_NUMBER() OVER (PARTITION BY (CASE WHEN ((`x` IS NULL) OR (`y` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `x`, `y` DESC)
END")
  )
})

test_that("win_rank(c()) gives an informative error", {
  expect_snapshot(error = TRUE, {
    translate_sql(row_number(c(x)))
  })
})

test_that("win_cumulative works", {
  local_con(simulate_dbi())
  sql_cumsum <- win_cumulative("SUM")

  expect_equal(
    sql_cumsum(ident("x"), "y"),
    sql("SUM(`x`) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)")
  )

  # NA values results in NA rank
  db <- memdb_frame(x = c(1, 2, NA, 3))
  expect_equal(
    db %>% mutate(rank = dense_rank(x)) %>% collect() %>% arrange(x),
    tibble(x = c(1:3, NA), rank = c(1:3, NA))
  )
})


# win_over ----------------------------------------------------------------

test_that("over() only requires first argument", {
  local_con(simulate_dbi())

  expect_equal(win_over("X"), sql("'X' OVER ()"))
})

test_that("multiple group by or order values don't have parens", {
  local_con(simulate_dbi())

  expect_equal(
    win_over(ident("x"), order = c("x", "y")),
    sql("`x` OVER (ORDER BY `x`, `y`)")
  )
  expect_equal(
    win_over(ident("x"), partition = c("x", "y")),
    sql("`x` OVER (PARTITION BY `x`, `y`)")
  )
})


# window_frame ------------------------------------------------------------

test_that("window_frame()", {
  lf <- lazy_frame(x = runif(10), y = 1:10)

  expect_snapshot(
    lf %>%
      window_frame(-3, 0) %>%
      window_order(x) %>%
      mutate(z = sum(y, na.rm = TRUE)) %>%
      show_query()
  )

  expect_snapshot(
    lf %>%
      window_frame(-3) %>%
      window_order(x) %>%
      mutate(z = sum(y, na.rm = TRUE)) %>%
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
  ) %>%
    group_by(part) %>%
    window_order(ord)

  lf1 <- lf %>%
    transmute(
      across(c(col1, col2), ~ sum(.x, na.rm = TRUE)),
      across(c(col3, col4), ~ order_by(desc(ord), cumsum(.x)))
    )

  sql_list <- get_select_sql(lf1$lazy_query$select, "mutate", op_vars(lf), simulate_sqlite())
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
      part = ident("part"),
      col1 = sql("SUM(`col1`) OVER `win1`"),
      col2 = sql("SUM(`col2`) OVER `win1`"),
      col3 = sql("SUM(`col3`) OVER `win2`"),
      col4 = sql("SUM(`col4`) OVER `win2`")
    )
  )

  # Different order does not confuse naming of windows
  lf2 <- lf %>%
    transmute(
      col1 = sum(col1, na.rm = TRUE),
      col3 = order_by(desc(ord), cumsum(col3)),
      col2 = sum(col2, na.rm = TRUE),
      col4 = order_by(desc(ord), cumsum(col4))
    )

  sql_list <- get_select_sql(lf2$lazy_query$select, "mutate", op_vars(lf), simulate_sqlite())
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
      part = ident("part"),
      col1 = sql("SUM(`col1`) OVER `win1`"),
      col3 = sql("SUM(`col3`) OVER `win2`"),
      col2 = sql("SUM(`col2`) OVER `win1`"),
      col4 = sql("SUM(`col4`) OVER `win2`")
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
  ) %>%
    group_by(part) %>%
    window_order(ord) %>%
    transmute(
      across(c(col1, col2), ~ sum(.x, na.rm = TRUE)),
      across(c(col3), ~ order_by(desc(ord), cumsum(.x)))
    )

  sql_list <- get_select_sql(lf$lazy_query$select, "mutate", op_vars(lf), simulate_sqlite())
  expect_equal(sql_list$window_sql, sql("`win1` AS (PARTITION BY `part`)"))
  expect_equal(
    sql_list$select_sql,
    sql(
      part = ident("part"),
      col1 = sql("SUM(`col1`) OVER `win1`"),
      col2 = sql("SUM(`col2`) OVER `win1`"),
      col3 = sql("SUM(`col3`) OVER (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING)")
    )
  )
})

test_that("name windows only if supported", {
  lf <- lazy_frame(
    col1 = runif(3),
    col2 = runif(3),
    part = c("a", "a", "b"),
    con = simulate_hana()
  ) %>%
    group_by(part) %>%
    transmute(
      across(c(col1, col2), ~ sum(.x, na.rm = TRUE))
    )

  sql_list <- get_select_sql(lf$lazy_query$select, "mutate", op_vars(lf), simulate_hana())
  expect_equal(sql_list$window_sql, character())
  expect_equal(
    sql_list$select_sql,
    sql(
      part = ident("part"),
      col1 = sql("SUM(`col1`) OVER (PARTITION BY `part`)"),
      col2 = sql("SUM(`col2`) OVER (PARTITION BY `part`)")
    )
  )
})
