context("test-translate-sql-window.r")

test_that("aggregation functions warn once if na.rm = FALSE", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))
  sql_mean <- win_aggregate("MEAN")

  expect_warning(sql_mean("x"), "Missing values")
  expect_warning(sql_mean("x"), NA)
  expect_warning(sql_mean("x", na.rm = TRUE), NA)
})

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
  expect_equal(translate_sql(first(x)), sql("FIRST_VALUE(`x`) OVER ()"))
  expect_equal(translate_sql(last(x)), sql("LAST_VALUE(`x`) OVER ()"))
  expect_equal(translate_sql(nth(x, 1)), sql("NTH_VALUE(`x`, 1) OVER ()"))
})

test_that("can override frame of recycled functions", {
  expect_equal(
    translate_sql(sum(x, na.rm = TRUE), vars_frame = c(-1, 0), vars_order = "y"),
    sql("SUM(`x`) OVER (ORDER BY `y` ROWS 1 PRECEDING)")
  )
})

# win_over ----------------------------------------------------------------

test_that("over() only requires first argument", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))

  expect_equal(win_over("X"), sql("'X' OVER ()"))
})

test_that("multiple group by or order values don't have parens", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))

  expect_equal(
    win_over(ident("x"), order = c("x", "y")),
    sql("`x` OVER (ORDER BY `x`, `y`)")
  )
  expect_equal(
    win_over(ident("x"), partition = c("x", "y")),
    sql("`x` OVER (PARTITION BY `x`, `y`)")
  )
})

