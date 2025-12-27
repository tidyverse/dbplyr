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