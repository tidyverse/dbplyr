test_that("sql_clause_select generates expected SQL", {
  expect_snapshot(sql_clause_select(sql("x", "y")))
  expect_snapshot(sql_clause_select(sql("x"), distinct = TRUE))
  expect_snapshot(sql_clause_select(sql("x"), top = 10))
  expect_snapshot(sql_clause_select(sql("x"), distinct = TRUE, top = 5))
})

test_that("clauses wrap long inputs nicely", {
  vars <- paste0("variable", seq_len(8))
  expect_snapshot(sql_clause_select(sql(vars)))
})

test_that("sql_clause_select requires non-empty input", {
  expect_snapshot(sql_clause_select(sql()), error = TRUE)
})

test_that("sql_clause_where wraps in parens and combines conditions with AND", {
  expect_snapshot(sql_clause_where(sql("x > 1")))
  expect_snapshot(sql_clause_where(sql("x > 1", "y < 2")))
})

test_that("sql_clause_order_by warns when dropping in subquery", {
  expect_snapshot(sql_clause_order_by(sql("x"), subquery = TRUE))
})

test_that("sql_select_clauses generates expected SQL", {
  expect_snapshot(
    sql_select_clauses(
      select = sql_clause_select(sql("x", "y")),
      from = sql_clause_from(sql("foo")),
      where = sql_clause_where(sql("x > 1")),
      group_by = sql_clause_group_by(sql("x")),
      having = sql_clause_having(sql("COUNT(*) > 1")),
      window = sql_clause_window(sql("win1 AS (PARTITION BY x)")),
      order_by = sql_clause_order_by(sql("x")),
      limit = sql_clause_limit(NULL, 10)
    )
  )
})

test_that("update clauses generate expected SQL", {
  expect_snapshot(sql_clause_update(sql("foo")))
  expect_snapshot(sql_clause_set(sql("x", "y"), sql("1", "2")))
})

test_that("insert clauses generate expected SQL", {
  expect_snapshot(sql_clause_insert(sql("x", "y")))
  expect_snapshot(sql_clause_insert(sql("x", "y"), into = sql("foo")))
})

test_that("sql_clause_where_exists generates expected SQL", {
  clause_true <- sql_clause_where_exists(sql("bar"), sql("x = y"), not = FALSE)
  clause_false <- sql_clause_where_exists(sql("bar"), sql("x = y"), not = TRUE)

  expect_snapshot({
    sql_format_clauses(clause_true)
    sql_format_clauses(clause_false)
  })
})
