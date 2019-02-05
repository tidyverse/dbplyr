#' Simulate database connections
#'
#' These functions generate S3 objects that have been designed to simulate
#' the action of a database connection, without actually having the database
#' available. Obviously, this simulation can only be incomplete, but most
#' importantly it allows us to simulate SQL generation for any database without
#' actually connecting to it.
#'
#' Simulated SQL always quotes identifies with `` `x` ``, and strings with
#' `'x'`.
#'
#' @keywords internal
#' @export
simulate_dbi <- function(class = character()) {
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
simulate_access <- function() simulate_dbi("ACCESS")

#' @export
#' @rdname simulate_dbi
simulate_hive <- function() simulate_dbi("Hive")

#' @export
#' @rdname simulate_dbi
simulate_mysql <- function() simulate_dbi("MySQLConnection")

#' @export
#' @rdname simulate_dbi
simulate_impala <- function() simulate_dbi("Impala")

#' @export
#' @rdname simulate_dbi
simulate_mssql <- function() simulate_dbi("Microsoft SQL Server")

#' @export
#' @rdname simulate_dbi
simulate_odbc <- function() simulate_dbi("OdbcConnection")

#' @export
#' @rdname simulate_dbi
simulate_oracle <- function() simulate_dbi("Oracle")

#' @export
#' @rdname simulate_dbi
simulate_postgres <- function() simulate_dbi("PostgreSQLConnection")

#' @export
#' @rdname simulate_dbi
simulate_sqlite <- function() simulate_dbi("SQLiteConnection")

#' @export
#' @rdname simulate_dbi
simulate_teradata <- function() simulate_dbi("Teradata")
