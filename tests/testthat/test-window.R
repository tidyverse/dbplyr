context("Window functions")


test_that("over() only requires first argument", {
  expect_equal(win_over("X"), sql("'X' OVER ()"))
})

test_that("multiple group by or order values don't have parens", {
  expect_equal(
    win_over(ident("x"), order = c("x", "y")),
    sql('"x" OVER (ORDER BY "x", "y")')
  )
  expect_equal(
    win_over(ident("x"), partition = c("x", "y")),
    sql('"x" OVER (PARTITION BY "x", "y")')
  )
})

test_that("connection affects quoting window function fields", {
  dbiTest <- structure(list(), class = "DBITestConnection")
  dbTest <- src_sql("test", con = dbiTest)
  testTable <- tbl_sql("test", src = dbTest, from = ident("table1"))

  out <- filter(group_by(testTable, field1), min_rank(desc(field1)) < 2)
  sqlText <- sql_render(out)

  expect_equal(
    grep(paste(
      "^SELECT `field1`",
      "FROM \\(SELECT `field1`, rank\\(\\) OVER \\(PARTITION BY `field1` ORDER BY `field1` DESC\\) AS `[a-zA-Z0-9]+`",
      "FROM `table1`\\) `[a-zA-Z0-9]+`",
      "WHERE \\(`[a-zA-Z0-9]+` < 2.0\\)$",
      sep = "\n"
    ), sqlText),
    1,
    info = sqlText
  )
})
