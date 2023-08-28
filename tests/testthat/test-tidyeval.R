test_that("simple expressions left as is", {
  lf <- lazy_frame(x = 1:10, y = 1:10)

  expect_equal(capture_dot(lf, NULL), NULL)
  expect_equal(capture_dot(lf, 10), 10)
  expect_equal(capture_dot(lf, x), sym("x"))
  expect_equal(capture_dot(lf, x + y), expr(x + y))
  expect_equal(capture_dot(lf, x[[1]]), expr(x[[1]]))

  # logicals
  expect_equal(eval(capture_dot(lf, T), globalenv()), TRUE)
  expect_equal(eval(capture_dot(lf, F), globalenv()), FALSE)
  expect_equal(capture_dot(lf, TRUE), TRUE)
  expect_equal(capture_dot(lf, FALSE), FALSE)
})

test_that("existing non-variables get inlined", {
  lf <- lazy_frame(x = 1:10, y = 1:10)

  n <- 10
  expect_equal(capture_dot(lf, x + n), expr(x + 10))
})

test_that("using environment of inlined quosures", {
  lf <- lazy_frame(x = 1:10, y = 1:10)

  n <- 10
  quo <- new_quosure(quote(x + n), env(n = 20))

  expect_equal(capture_dot(lf, f(!!quo)), quote(f(x + 20)))
})

test_that("namespace operators always evaluated locally", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(partial_eval(quote(base::sum(1, 2)), lf), 3)
  expect_equal(partial_eval(quote(base:::sum(1, 2)), lf), 3)
})

test_that("namespaced calls to dplyr functions are stripped", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(partial_eval(quote(dplyr::n()), lf), expr(n()))
  # hack to avoid check complaining about not declared imports
  expect_equal(partial_eval(rlang::parse_expr("stringr::str_to_lower(x)"), lf), expr(str_to_lower(x)))
  expect_equal(partial_eval(rlang::parse_expr("lubridate::today()"), lf), expr(today()))
  expect_equal(partial_eval(rlang::parse_expr("clock::add_years(x, 1)"), lf), expr(add_years(x, 1)))
})

test_that("use quosure environment for unevaluted formulas", {
  lf <- lazy_frame(x = 1, y = 2)

  z <- 1
  expect_equal(partial_eval(expr(~z), lf), quote(~1))
})

test_that("can look up inlined function", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(
    partial_eval(expr((!!mean)(x)), data = lf),
    expr(mean(x))
  )
})

test_that("respects tidy evaluation pronouns", {
  lf <- lazy_frame(x = 1, y = 2)

  x <- "X"
  X <- "XX"

  expect_equal(partial_eval(expr(.data$x), lf), expr(x))
  expect_equal(partial_eval(expr(.data[["x"]]), lf), expr(x))
  expect_equal(partial_eval(expr(.data[[x]]), lf), expr(X))

  expect_equal(partial_eval(expr(.env$x), lf), "X")
  expect_equal(partial_eval(expr(.env[["x"]]), lf), "X")
  expect_equal(partial_eval(expr(.env[[x]]), lf), "XX")
})

test_that("fails with multi-classes", {
  lf <- lazy_frame(x = 1, y = 2)
  x <- structure(list(), class = c('a', 'b'))
  expect_error(partial_eval(x, lf), "Unknown input type", fixed = TRUE)
})
