# build_sql() ------------------------------------------------------------------

test_that("build_sql() requires connection", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- ident("TABLE")
  expect_snapshot(error = TRUE, build_sql("SELECT * FROM ", x))
})

# sql_expr() -------------------------------------------------------------------

test_that("NULL becomes SQL NULL", {
  con <- dialect_ansi()
  expect_equal(sql_expr(NULL), sql("NULL"))
})

test_that("atomic vectors are escaped", {
  con <- dialect_ansi()

  expect_equal(sql_expr(2, con = con), sql("2.0"))
  expect_equal(sql_expr("x", con = con), sql("'x'"))
})

test_that("user infix functions have % stripped", {
  con <- dialect_ansi()

  expect_equal(sql_expr(x %like% y, con = con), sql("x LIKE y"))
})

test_that("string function names are not quoted", {
  con <- dialect_ansi()

  f <- "foo"
  expect_equal(sql_expr((!!f)(), con = con), sql("FOO()"))
})

test_that("correct number of parens", {
  con <- dialect_ansi()

  expect_equal(sql_expr((1L), con = con), sql("(1)"))
})

# as.sql() ---------------------------------------------------------------------

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

# ident_q() --------------------------------------------------------------------

test_that("quoted identifier correctly escaped", {
  con <- dialect_ansi()
  x2 <- ident_q('"x"')
  expect_equal(escape(x2, con = con), sql('"x"'))

  expect_equal(sql_vector(ident_q(), collapse = NULL, con = con), sql())
  expect_equal(
    sql_vector(ident_q(), parens = FALSE, collapse = "", con = con),
    sql("")
  )
  expect_equal(
    sql_vector(ident_q(), parens = TRUE, collapse = "", con = con),
    sql("()")
  )
})
