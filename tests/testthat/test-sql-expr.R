context("test-sql-expr.R")

test_that("atomic vectors are escaped", {
  expect_equal(sql_expr(2), sql("2.0"))
  expect_equal(sql_expr("x"), sql("'x'"))
})

test_that("user infix functions have % stripped", {
  expect_equal(sql_expr(x %like% y), sql("x LIKE y"))
})

test_that("string function names are not quoted", {
  f <- "foo"
  expect_equal(sql_expr(UQ(f)()), sql("FOO()"))
})

test_that("correct number of parens", {
  expect_equal(sql_expr((1L)), sql("(1)"))
})
