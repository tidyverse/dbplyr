test_that("sql_select_clauses generates expected SQL", {
  con <- simulate_dbi()

  clauses <- list(
    sql_clause_select(sql("a", "b", "c")),
    sql_clause_from(sql("table")),
    sql_clause_where(sql("x > 1", "y < 2")),
    sql_clause_group_by(sql('"a"', '"b"')),
    sql_clause_having(sql("COUNT(*) > 5")),
    sql_clause_window(sql('"win1" AS (PARTITION BY "a")')),
    sql_clause_order_by(sql('"a" DESC')),
    sql_clause_limit(10)
  )
  expect_snapshot(sql_format_clauses(clauses, 0, con))
})

test_that("sql_select_clauses can generate multiple lines", {
  con <- simulate_dbi()

  clauses <- list(
    sql_clause_select(sql(paste0("variable", 1:8))),
    sql_clause_from(sql("table"))
  )

  expect_snapshot(sql_format_clauses(clauses, 0, con))
})

test_that("sql_clause_select can generate top", {
  expect_equal(
    format(sql_clause_select(sql("table"), top = 5)),
    "SELECT TOP 5 table"
  )
})
