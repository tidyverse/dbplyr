context("win_over")

test_that("over() only requires first argument", {
  expect_equal(win_over("X"), sql("'X' OVER ()"))
})

test_that("multiple group by or order values don't have parens", {
  expect_equal(
    win_over(ident("x"), order = c("x", "y")),
    sql('"x" OVER (ORDER BY "x", "y")')
  )
  expect_equal(
    win_over(ident("x"), partition = c("x", "y")),
    sql('"x" OVER (PARTITION BY "x", "y")')
  )
})

test_that("connection affects quoting window function fields", {
  old <- set_current_con(simulate_test())
  on.exit(set_current_con(old))

  expect_equal(win_over(ident("x")), sql("`x` OVER ()"))
  expect_equal(
    win_over(ident("x"), partition = "x"),
    sql("`x` OVER (PARTITION BY `x`)")
  )
  expect_equal(
    win_over(ident("x"), order = "x"),
    sql("`x` OVER (ORDER BY `x`)")
  )
})
