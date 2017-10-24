context("schema")

# Create database with a schema
sqlite_con_with_aux <- function() {
  tmp <- tempfile()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))

  con
}

test_that("can refer to default schema explicitly", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t1 (x)")

  expect_equal(tbl_vars(tbl(con, "t1")), "x")
  expect_equal(tbl_vars(tbl(con, in_schema("main", "t1"))), "x")
})

test_that("can distinguish 'schema.table' from 'schema'.'table'", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE aux.t1 (x, y, z)")
  DBI::dbExecute(con, "CREATE TABLE 'aux.t1' (a, b, c)")

  expect_equal(tbl_vars(tbl(con, in_schema("aux", "t1"))), c("x", "y", "z"))
  expect_equal(tbl_vars(tbl(con, ident("aux.t1"))), c("a", "b", "c"))
})

test_that("can create a new table in non-default schema", {
  con <- sqlite_con_with_aux()
  on.exit(DBI::dbDisconnect(con))
  aux_mtcars <- copy_to(con, mtcars, in_schema("aux", "mtcars"), temporary = FALSE)

  expect_equal(tbl_vars(aux_mtcars), tbl_vars(mtcars))
})
