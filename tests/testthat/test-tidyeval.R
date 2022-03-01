test_that("namespace operators always evaluated locally", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(db_squash(quote(base::sum(1, 2)), lf), 3)
  expect_equal(db_squash(quote(base:::sum(1, 2)), lf), 3)
})

test_that("namespaced calls to dplyr functions are stripped", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(db_squash(quote(dplyr::n()), lf), expr(n()))
})

test_that("use quosure environment for unevaluted formulas", {
  lf <- lazy_frame(x = 1, y = 2)

  z <- 1
  expect_equal(db_squash(expr(~z), lf), quote(~1))
})

test_that("can look up inlined function", {
  lf <- lazy_frame(x = 1, y = 2)

  expect_equal(
    db_squash(expr((!!mean)(x)), data = lf),
    expr(mean(x))
  )
})

test_that("respects tidy evaluation pronouns", {
  lf <- lazy_frame(x = 1, y = 2)

  x <- "X"
  X <- "XX"

  expect_equal(db_squash(expr(.data$x), lf), expr(x))
  expect_equal(db_squash(expr(.data[["x"]]), lf), expr(x))
  expect_equal(db_squash(expr(.data[[x]]), lf), expr(X))

  expect_equal(db_squash(expr(.env$x), lf), "X")
  expect_equal(db_squash(expr(.env[["x"]]), lf), "X")
  expect_equal(db_squash(expr(.env[[x]]), lf), "XX")
})

test_that("fails with multi-classes", {
  lf <- lazy_frame(x = 1, y = 2)
  x <- structure(list(), class = c('a', 'b'))
  expect_error(db_squash(x, lf), "Unknown input type", fixed = TRUE)
})
