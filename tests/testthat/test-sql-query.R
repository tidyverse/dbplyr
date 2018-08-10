context("test-sql-query.R")

test_that("add_suffixes works if no suffix requested", {
  expect_equal(add_suffixes(c("x", "x"), "y", ""), c("x", "x"))
  expect_equal(add_suffixes(c("x", "y"), "y", ""), c("x", "y"))
})

test_that("select_query() print method output is as expected", {
  mf <- select_query(lazy_frame(x = 1, src = simulate_dbi()))
  expect_equal(
    paste0(
      capture.output(print.select_query(mf)),
      collapse = " "
    ),
    "<SQL SELECT> From:     df Select:   *"
  )
})
