test_that("namespace operators always evaluated locally", {
  expect_equal(partial_eval(quote(base::sum(1, 2))), 3)
  expect_equal(partial_eval(quote(base:::sum(1, 2))), 3)
})

test_that("namespaced calls to dplyr functions are stripped", {
  expect_equal(partial_eval(quote(dplyr::n())), expr(n()))
})

test_that("use quosure environment for unevaluted formulas", {
  x <- 1
  expect_equal(partial_eval(expr(~x)), quote(~1))
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

test_that("fails with multi-classes", {
  x <- structure(list(), class = c('a', 'b'))
  expect_error(partial_eval(x), "Unknown input type", fixed = TRUE)
})

# across() ----------------------------------------------------------------

test_that("across() translated to individual components", {
  # test partial_eval_across() indirectly via SQL generation
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(everything(), "log")))
  expect_snapshot(lf %>% summarise(across(everything(), log)))
  expect_snapshot(lf %>% summarise(across(everything(), list(log))))

  expect_snapshot(lf %>% summarise(across(everything(), "log", base = 2)))

  expect_snapshot(lf %>% summarise(across(everything(), c("log", "exp"))))
  expect_snapshot(lf %>% summarise(across(everything(), c("log", "exp"), .names = "{.fn}_{.col}")))
})
