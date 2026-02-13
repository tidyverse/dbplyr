test_that("check_sql() checks inputs", {
  f <- function(x) check_sql(x)

  expect_snapshot(f("x"), error = TRUE)
  expect_no_error(f(sql("x")))
})

test_that("check_sql() optionally checks names", {
  f <- function(x) check_sql(x, allow_names = FALSE)

  expect_no_error(f(sql("a")))
  expect_snapshot(f(sql(a = "x")), error = TRUE)
})

test_that("check_sql() optionally allows NULL", {
  f <- function(x) check_sql(x, allow_null = TRUE)

  expect_no_error(f(NULL))
  expect_snapshot(f("x"), error = TRUE)
})
