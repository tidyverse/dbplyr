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
    sql("SELECT COUNT(*) FROM `abc` AS `master`")
  )
})
