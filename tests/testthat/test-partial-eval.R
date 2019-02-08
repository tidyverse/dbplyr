context("test-partial-eval.R")

test_that("namespace operators always evaluated locally", {
  expect_equal(partial_eval(quote(base::sum(1, 2))), 3)
  expect_equal(partial_eval(quote(base:::sum(1, 2))), 3)
})

test_that("namespaced calls to dplyr functions are stripped", {
  expect_equal(partial_eval(quote(dplyr::n())), expr(n()))
})

test_that("use quosure environment for unevaluted formulas", {
  x <- 1
  expect_equal(partial_eval(expr(~x)), ~1)
})

test_that("can look up inlined function", {
  expect_equal(
    partial_eval(expr((!!mean)(x)), vars = "x"),
    expr(mean(x))
  )

  expect_equal(
    partial_eval(expr((!!as_function("mean"))(x)), vars = "x"),
    expr(mean(x))
  )
})

test_that("respects tidy evaluation pronouns", {
  x <- "X"
  X <- "XX"

  expect_equal(partial_eval(expr(.data$x)), expr(x))
  expect_equal(partial_eval(expr(.data[["x"]])), expr(x))
  expect_equal(partial_eval(expr(.data[[x]])), expr(X))

  expect_equal(partial_eval(expr(.env$x)), "X")
  expect_equal(partial_eval(expr(.env[["x"]])), "X")
  expect_equal(partial_eval(expr(.env[[x]])), "XX")
})
