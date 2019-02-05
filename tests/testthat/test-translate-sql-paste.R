context("test-translate-sql-paste.R")

test_that("basic prefix operation", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))

  paste <- sql_paste("")
  x <- ident("x")
  y <- ident("y")

  expect_equal(paste(x), sql("CONCAT_WS('', `x`)"))
  expect_equal(paste(x, y), sql("CONCAT_WS('', `x`, `y`)"))
  expect_equal(paste(x, y, sep = " "), sql("CONCAT_WS(' ', `x`, `y`)"))
})


test_that("basic infix operation", {
  old <- set_current_con(simulate_dbi())
  on.exit(set_current_con(old))

  paste <- sql_paste_infix("", "&&", function(x) sql_expr(cast((!!x) %as% text)))
  x <- ident("x")
  y <- ident("y")

  expect_equal(paste(x), sql("CAST(`x` AS text)"))
  expect_equal(paste(x, y), sql("`x` && `y`"))
  expect_equal(paste(x, y, sep = " "), sql("`x` && ' ' && `y`"))
})
