test_that("zero length inputs return correct clases", {
  expect_s3_class(ident(), "ident")
})

test_that("ident quotes", {
  con <- dialect_ansi()
  x1 <- ident("x")

  expect_equal(escape(x1, con = con), sql('"x"'))
})

test_that("can format ident", {
  expect_snapshot(ident())
})
