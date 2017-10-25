context("translate-window")

test_that("window functions without group have empty over", {
  expect_equal(translate_sql(n()), sql("COUNT(*) OVER ()"))
  expect_equal(translate_sql(sum(x, na.rm = TRUE)), sql('sum("x") OVER ()'))
})

test_that("aggregating window functions ignore order_by", {
  expect_equal(
    translate_sql(n(), vars_order = "x"),
    sql("COUNT(*) OVER ()")
  )
  expect_equal(
    translate_sql(sum(x, na.rm = TRUE), vars_order = "x"),
    sql('sum("x") OVER ()')
  )
})

test_that("order_by overrides default ordering", {
  expect_equal(
    translate_sql(order_by(y, cumsum(x)), vars_order = "x"),
    sql('sum("x") OVER (ORDER BY "y" ROWS UNBOUNDED PRECEDING)')
  )
})

test_that("cumulative windows warn if no order", {
  expect_warning(translate_sql(cumsum(x)), "does not have explicit order")
  expect_warning(translate_sql(cumsum(x), vars_order = "x"), NA)
})

test_that("ntile always casts to integer", {
  expect_equal(
    translate_sql(ntile(x, 10.5)),
    sql('NTILE(10) OVER (ORDER BY "x")')
  )
})

test_that("first, last, and nth translated to _value", {
  expect_equal(translate_sql(first(x)), sql('first_value("x") OVER ()'))
  expect_equal(translate_sql(last(x)), sql('last_value("x") OVER ()'))
  expect_equal(translate_sql(nth(x, 1)), sql('nth_value("x", 1) OVER ()'))
})


test_that("can override frame of recycled functions", {
  expect_equal(
    translate_sql(sum(x, na.rm = TRUE), vars_frame = c(-1, 0), vars_order = "y"),
    sql('sum("x") OVER (ORDER BY "y" ROWS 1 PRECEDING)')
  )

})
