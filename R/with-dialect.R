#' Override the SQL dialect for a connection
#'
#' @description
#' `with_dialect()` wraps a database connection to use a specific SQL dialect
#' for SQL generation, while still using the original connection for database
#' operations.
#'
#' This is useful when you want to use a connection with an ODBC or JDBC driver
#' but want dbplyr to generate SQL for a specific database backend.
#'
#' @param con A database connection (class `DBIConnection`).
#' @param dialect A dialect object created by a `dialect_*()` function
#'   (e.g., [dialect_postgres()], [dialect_sqlite()]).
#' @return A connection object that uses the specified dialect for SQL
#'   generation.
#' @export
#' @examples
#' # Wrap an in-memory SQLite connection to use Postgres dialect
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' pg_con <- with_dialect(con, dialect_postgres())
#'
#' # SQL generation uses Postgres syntax
#' lf <- lazy_frame(x = 1, con = pg_con)
#' lf |> dplyr::mutate(y = sd(x))
#'
#' DBI::dbDisconnect(con)
with_dialect <- function(con, dialect) {
  check_con(con)
  if (!is_sql_dialect(dialect)) {
    cli_abort("{.arg dialect} must be a dialect object.")
  }

  attr(con, "dbplyr_dialect") <- dialect
  con
}
