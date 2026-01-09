test_that("2nd edition uses sql methods", {
  con <- structure(list(), class = c("Test", "DBIConnection"))

  local_methods(
    dbplyr_edition.Test = \(con) 2,
    sql_table_analyze.Test = \(con, ...) abort("sql_method")
  )
  expect_error(dbplyr_analyze(con), "sql_method")
})

test_that("sql_query_rows() works", {
  expect_equal(
    sql_query_rows(simulate_dbi(), ident("abc")),
    sql('SELECT COUNT(*) FROM "abc" AS "master"')
  )
})

test_that("analyse/explain sql generates expected SQL", {
  con <- simulate_dbi()
  expect_snapshot(sql_table_analyze(con, "tbl"))
  expect_snapshot(sql_query_explain(con, sql("SELECT * FROM foo")))
})

test_that("sql_query_wrap generates expected SQL", {
  unique_subquery_name_reset()

  con <- simulate_dbi()
  expect_snapshot(sql_query_wrap(con, ident("table")))
  expect_snapshot(sql_query_wrap(con, in_schema("schema", "tbl")))
  expect_snapshot(sql_query_wrap(con, sql("SELECT * FROM foo")))
})

test_that("sql_table_index generates expected SQL", {
  con <- simulate_dbi()
  expect_snapshot(sql_table_index(con, "tbl", c("a", "b")))
  expect_snapshot(sql_table_index(con, "tbl", "c", unique = TRUE))
})

test_that("sql_query_save generates expected SQL", {
  con <- simulate_dbi()

  sql <- sql("SELECT * FROM foo")
  expect_snapshot(sql_query_save(con, sql, "tbl"))
  expect_snapshot(sql_query_save(con, sql, "tbl", temporary = FALSE))
})
