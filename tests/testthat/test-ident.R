context("test-ident.R")

test_that("zero length inputs return correct clases", {
  expect_s3_class(ident(), "ident")
  expect_s3_class(ident_q(), "ident_q")
})

test_that("ident quotes and ident_q doesn't", {
  con <- simulate_dbi()
  x1 <- ident("x")
  x2 <- ident_q('"x"')

  expect_equal(escape(x1, con = con), sql('`x`'))
  expect_equal(escape(x2, con = con), sql('"x"'))
})

test_that("ident are left unchanged when coerced to sql", {
  x1 <- ident("x")
  x2 <- ident_q('"x"')

  expect_equal(as.sql(x1), x1)
  expect_equal(as.sql(x2), x2)
})
