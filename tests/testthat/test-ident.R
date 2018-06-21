context("ident")

test_that("zero length inputs return correct clases", {
  expect_s3_class(ident(), "ident")
  expect_s3_class(ident_q(), "ident_q")
})

test_that("ident quotes and ident_q doesn't", {
  x1 <- ident("x")
  x2 <- ident_q('"x"')

  expect_equal(escape(x1), sql('"x"'))
  expect_equal(escape(x2), sql('"x"'))
})

test_that("ident are left unchanged when coerced to sql", {
  x1 <- ident("x")
  x2 <- ident_q('"x"')

  expect_equal(as.sql(x1), x1)
  expect_equal(as.sql(x2), x2)
})
