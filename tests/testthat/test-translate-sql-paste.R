context("test-translate-sql-paste.R")

test_that("basic infix operation", {
  sql_paste <- sql_paste_infix("", "&&", function(x) sql_expr(cast(UQ(x) %as% text)))
  x <- ident("x")
  y <- ident("y")

  expect_equal(sql_paste(x), sql('CAST("x" AS text)'))
  expect_equal(sql_paste(x, y), sql('"x" && "y"'))
  expect_equal(sql_paste(x, y, sep = " "), sql('"x" && \' \' && "y"'))
})
