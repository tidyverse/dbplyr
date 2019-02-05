context("win_over")

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
