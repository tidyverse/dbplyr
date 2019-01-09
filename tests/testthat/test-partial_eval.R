context("partial_eval")

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
