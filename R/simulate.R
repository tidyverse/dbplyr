#' Simulate database connections
#'
#' These functions generate S3 objects that have been designed to simulate
#' the action of a database connection, without actually having the database
#' available. Obviously, this simulation can only be incomplete, but most
#' importantly it allows us to simulate SQL generation for any database without
#' actually connecting to it.
#'
#' @keywords internal
#' @export
simulate_dbi <- function(class = character()) {
  structure(
    list(),
    class = c(class, "DBIConnection")
  )
}

#' @export
#' @rdname simulate_dbi
simulate_test <- function(class = character()) {
  structure(
    list(),
    class = c(class, "TestConnection", "DBIConnection")
  )
}

# Needed to work around fundamental hackiness of how I'm mingling
# S3 and S4 dispatch
sql_escape_ident.TestConnection <- function(con, x) {
  sql_quote(x, "`")
}

sql_escape_string.TestConnection <- function(con, x) {
  sql_quote(x, "'")
}

#' @export
#' @rdname simulate_dbi
simulate_access <- function() simulate_test("ACCESS")

#' @export
#' @rdname simulate_dbi
simulate_hive <- function() simulate_test("Hive")

#' @export
#' @rdname simulate_dbi
simulate_mysql <- function() simulate_test("MySQLConnection")

#' @export
#' @rdname simulate_dbi
simulate_impala <- function() simulate_test("Impala")

#' @export
#' @rdname simulate_dbi
simulate_mssql <- function() simulate_test("Microsoft SQL Server")

#' @export
#' @rdname simulate_dbi
simulate_oracle <- function() simulate_test("Oracle")

#' @export
#' @rdname simulate_dbi
simulate_postgres <- function() simulate_test("PostgreSQLConnection")

#' @export
#' @rdname simulate_dbi
simulate_sqlite <- function() simulate_test("SQLiteConnection")

#' @export
#' @rdname simulate_dbi
simulate_teradata <- function() simulate_test("Teradata")
