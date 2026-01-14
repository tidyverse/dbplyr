#' Simulate database connections
#'
#' @description
#' These functions generate S3 objects that have been designed to simulate
#' the action of a database connection, without actually having the database
#' available. Obviously, this simulation can only be incomplete, but most
#' importantly it allows us to simulate SQL generation for any database without
#' actually connecting to it.
#'
#' Simulated SQL quotes identifiers with `"x"` (double quotes) by default,
#' `` `x` `` (backticks) for MySQL/MariaDB/SQLite, and `[x]` (square brackets)
#' for SQL Server. Strings are quoted with `'x'`.
#'
#' @keywords internal
#' @export
simulate_dbi <- function(class = character(), ...) {
  structure(
    list(),
    ...,
    class = c(class, "TestConnection", "DBIConnection")
  )
}

dialect_ansi <- function() {
  structure(
    list(
      quote_identifier = function(x) sql_quote(x, '"'),
      has = list(
        window_clause = FALSE,
        table_alias_with_as = TRUE
      )
    ),
    class = "sql_dialect"
  )
}

#' @export
dbplyr_edition.TestConnection <- function(con) 2L

#' @export
sql_escape_ident.TestConnection <- function(con, x) {
  if (inherits(con, "Microsoft SQL Server")) {
    sql_quote(x, c("[", "]"))
  } else if (
    inherits(con, "MySQLConnection") ||
      inherits(con, "MariaDBConnection") ||
      inherits(con, "SQLiteConnection")
  ) {
    sql_quote(x, "`")
  } else {
    sql_quote(x, '"')
  }
}
