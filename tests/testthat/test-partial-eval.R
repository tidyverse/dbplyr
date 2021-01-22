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
# test partial_eval_across() indirectly via SQL generation

test_that("across() translates character vectors", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, "log")))
  expect_snapshot(lf %>% summarise(across(a:b, "log", base = 2)))

  expect_snapshot(lf %>% summarise(across(a, c("log", "exp"))))

  out <- lf %>% summarise(across(a:b, c(x = "log", y = "exp")))
  expect_equal(colnames(out), c("a_x", "a_y", "b_x", "b_y"))
})

test_that("across() translates functions", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, log)))
  expect_snapshot(lf %>% summarise(across(a:b, log, base = 2)))

  expect_snapshot(lf %>% summarise(across(a:b, list(log, exp))))

  out <- lf %>% summarise(across(a:b, list(x = log, y = exp)))
  expect_equal(colnames(out), c("a_x", "a_y", "b_x", "b_y"))
})

test_that("untranslatable functions are preserved", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, SQL_LOG)))
})

test_that("across() translates formulas", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% summarise(across(a:b, ~ log(.x, 2))))
  expect_snapshot(lf %>% summarise(across(a:b, list(~ log(.x, 2)))))
})

test_that("across() translates NULL", {
  lf <- lazy_frame(a = 1, b = 2)
  expect_snapshot(lf %>% mutate(across(a:b)))
})

test_that("can control names", {
  lf <- lazy_frame(a = 1, b = 2)
  out <- lf %>% summarise(across(a:b, c("log", "exp"), .names = "{.fn}_{.col}"))
  expect_equal(colnames(out), c("log_a", "exp_a", "log_b", "exp_b"))
})

test_that("old _at functions continue to work", {
  withr::local_options(lifecycle_verbosity = "quiet")
  lf <- lazy_frame(a = 1, b = 2)

  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), "sum"))
  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), sum))
  expect_snapshot(lf %>% dplyr::summarise_at(dplyr::vars(a:b), ~ sum(.)))
})
