test_that("2nd edition uses sql methods", {
  reset_warning_verbosity("Test-edition")
  local_methods(
    db_analyze.Test = \(con, ...) abort("db_method")
  )

  con <- structure(list(), class = c("Test", "DBIConnection"))
  expect_snapshot(expect_error(dbplyr_analyze(con), "db_method"))

  local_methods(
    dbplyr_edition.Test = \(con) 2,
    sql_table_analyze.Test = \(con, ...) abort("sql_method")
  )
  expect_error(dbplyr_analyze(con), "sql_method")
})

test_that("sql_query_rows() works", {
  expect_equal(
    sql_query_rows(simulate_dbi(), ident("abc")),
    sql("SELECT COUNT(*) FROM `abc` AS `master`")
  )
})

test_that("handles DBI error", {
  unique_subquery_name_reset()
  con <- local_sqlite_connection()

  expect_snapshot(
    {
      (expect_error(db_analyze(con, "tbl")))
      (expect_error(db_create_index(con, "tbl", "col")))

      (expect_error(db_explain(con, "invalid sql")))
      (expect_error(db_query_fields(con, "does not exist")))
      (expect_error(db_save_query(con, "invalid sql", "tbl")))
    },
    transform = snap_transform_dbi
  )
})
