context("test-sql-expr.R")

test_that("atomic vectors are escaped", {
  con <- simulate_dbi()

  expect_equal(sql_expr(2, con = con), sql("2.0"))
  expect_equal(sql_expr("x", con = con), sql("'x'"))
})

test_that("user infix functions have % stripped", {
  con <- simulate_dbi()

  expect_equal(sql_expr(x %like% y, con = con), sql("x LIKE y"))
})

test_that("string function names are not quoted", {
  con <- simulate_dbi()

  f <- "foo"
  expect_equal(sql_expr((!!f)(), con = con), sql("FOO()"))
})

test_that("correct number of parens", {
  con <- simulate_dbi()

  expect_equal(sql_expr((1L), con = con), sql("(1)"))
})
