context("test-sql")

test_that("can concatenate sql vector without supplying connection", {
  expect_equal(c(sql("x")), sql("x"))
  expect_equal(c(sql("x"), "x"), sql("x", "'x'"))
  expect_equal(c(ident("x")), sql("`x`"))
})
