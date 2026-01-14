test_that("sql_quote() returns empty sql() for empty input", {
  expect_equal(sql_quote(character(), "'"), sql())
})

test_that("sql_quote() converts NA to NULL", {
  expect_equal(sql_quote(NA_character_, "'"), sql("NULL"))
  expect_equal(sql_quote(c("x", NA), "'"), sql("'x'", "NULL"))
})

test_that("sql_quote() preserves names", {
  expect_equal(sql_quote(c(a = "x", b = "y"), "'"), sql(a = "'x'", b = "'y'"))
})

test_that("sql_quote() doubles quote characters in input", {
  expect_equal(sql_quote("it's", "'"), sql("'it''s'"))
  expect_equal(sql_quote("a]b", c("[", "]")), sql("[a]]b]"))
})

test_that("sql_quote() checks its inputs", {
  expect_snapshot(error = TRUE, {
    sql_quote(1)
    sql_quote("x", 1)
    sql_quote("x", character())
  })
})
