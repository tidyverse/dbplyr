test_that("sql() creates sql object from character inputs", {
  expect_s3_class(sql("x"), "sql")
  expect_equal(sql("x", "y"), sql(c("x", "y")))
})

test_that("sql() preserves names", {
  x <- sql(a = "x", b = "y")
  expect_equal(names(x), c("a", "b"))
})

test_that("sql() is idempotent", {
  expect_equal(sql(sql("x")), sql("x"))
  expect_equal(sql(sql("x"), sql("y")), sql("x", "y"))
})

test_that("is.sql() checks for sql class", {
  expect_true(is.sql(sql("x")))
  expect_false(is.sql("x"))
  expect_false(is.sql(1))
})

test_that("subsetting preserves sql class", {
  x <- sql("a", "b", "c")
  expect_s3_class(x[1], "sql")
  expect_s3_class(x[1:2], "sql")
  expect_s3_class(x[[1]], "sql")
})

test_that("can format sql", {
  expect_snapshot(sql())
  expect_snapshot(sql(a = "x", "y"))
})

# as.sql() -------------------------

test_that("as.sql() is deprecated", {
  expect_snapshot(as.sql(ident("x")))
})

test_that("as.sql() only affects character vectors", {
  local_options(lifecycle_verbosity = "quiet")

  expect_equal(as.sql("x"), ident("x"))

  x1 <- ident("x")
  expect_equal(as.sql(x1), x1)

  x2 <- ident_q('"x"')
  expect_equal(as.sql(x2), x2)

  x3 <- sql("SELECT 1")
  expect_equal(as.sql(x3), x3)
})
