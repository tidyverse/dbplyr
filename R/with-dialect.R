#' Override the SQL dialect for a connection
#'
#' @description
#' `with_dialect()` overrides the default dialect assigned to a connection.
#' This is useful when dbplyr guesses the dialect incorrectly, which is most
#' likely to occur with ODBC/JDBC/ADBC backends.
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

  # This is a little hacky, but it seems to be ok to add additional attributes
  # to S4 objects, and namespacing the attribute ensures that there's little
  # chance of collision.
  attr(con, "dbplyr_dialect") <- dialect
  con
}
