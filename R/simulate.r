simulate_test <- function() {
  structure(list(), class = c("DBITestConnection", "DBIConnection"))
}

db_query_fields.DBITestConnection <- function(con, sql, ...) {
  c("field1")
}

sql_escape_ident.DBITestConnection <- function(con, x) {
  sql_quote(x, "`")
}

# DBI connections --------------------------------------------------------------

#' @export
#' @rdname tbl_lazy
simulate_dbi <- function() {
  structure(
    list(),
    class = "DBIConnection"
  )
}

#' @export
#' @rdname tbl_lazy
simulate_sqlite <- function() {
  structure(
    list(),
    class = c("SQLiteConnection", "DBIConnection")
  )
}

#' @export
#' @rdname tbl_lazy
simulate_postgres <- function() {
  structure(
    list(),
    class = c("PostgreSQLConnection", "DBIConnection")
  )
}

simulate_mssql <- function() {

  class_mssql <- new.env(parent = emptyenv())

  setClass(
    "OdbcConnection",
    contains = "DBIConnection",
    slots = list(quote = "character"),
    where = class_mssql)

  dbms.name <- "Microsoft SQL Server"

  setClass(dbms.name, contains = "OdbcConnection", where = class_mssql)
  new(dbms.name, quote = "\"")
}
