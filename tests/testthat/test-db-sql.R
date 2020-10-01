test_that("2nd edition uses sql methods", {
  local_methods(
    db_analyze.Test = function(con, ...) stop("db_method")
  )

  con <- structure(list(), class = c("Test", "DBIConnection"))
  expect_error(dbplyr_analyze(con), "db_method")

  local_methods(
    dbplyr_edition.Test = function(con) 2,
    sql_table_analyze.Test = function(con, ...) stop("sql_method")
  )
  expect_error(dbplyr_analyze(con), "sql_method")
})
