test_that("sql_quote() returns empty sql() for empty input", {
  expect_equal(sql_quote(character(), "'"), sql())
  expect_equal(sql_quote(character(), "'", backslash = TRUE), sql())
})

test_that("sql_quote() converts NA to NULL", {
  expect_equal(sql_quote(NA_character_, "'"), sql("NULL"))
  expect_equal(sql_quote(c("x", NA), "'"), sql("'x'", "NULL"))
  expect_equal(sql_quote(c("x", NA), "'", backslash = TRUE), sql("'x'", "NULL"))
})

test_that("sql_quote() preserves names", {
  expect_equal(sql_quote(c(a = "x", b = "y"), "'"), sql(a = "'x'", b = "'y'"))
  expect_equal(
    sql_quote(c(a = "it's", b = "a\\b"), "'", backslash = TRUE),
    sql(a = "'it\\'s'", b = "'a\\\\b'")
  )
})

test_that("sql_quote() doubles quote characters in input", {
  expect_equal(sql_quote("it's", "'"), sql("'it''s'"))
  expect_equal(sql_quote("a]b", c("[", "]")), sql("[a]]b]"))
  expect_equal(sql_quote("a\\b", "'"), sql("'a\\b'"))
})

test_that("sql_quote() can escape quotes and backslashes with backslashes", {
  expect_equal(
    sql_quote(c("it's", "a\\b", "\\'", "\\"), "'", backslash = TRUE),
    sql("'it\\'s'", "'a\\\\b'", "'\\\\\\''", "'\\\\'")
  )
  expect_equal(
    sql_quote("a]b\\", c("[", "]"), backslash = TRUE),
    sql("[a\\]b\\\\]")
  )
})

test_that("sql_quote() leaves DBI SQL fragments unchanged", {
  x <- c(a = "'a\\b'", b = "CURRENT_TIMESTAMP")
  fragment <- DBI::SQL(x, names = names(x))

  expect_equal(sql_quote(fragment, "'"), sql(x))
  expect_equal(sql_quote(fragment, "'", backslash = TRUE), sql(x))
  expect_equal(sql_quote(fragment, c("[", "]")), sql(x))
})

test_that("sql_quote() checks its inputs", {
  expect_snapshot(error = TRUE, {
    sql_quote(1)
    sql_quote("x", 1)
    sql_quote("x", character())
    sql_quote("x", "'", backslash = NA)
  })
})
