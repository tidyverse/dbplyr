#' @include backend-postgres.R
NULL

# nocov start
# RPostgreSQL connections use the postgres dialect
#' @export
dbplyr_edition.PostgreSQLConnection <- function(con) {
  2L
}

#' @export
sql_dialect.PostgreSQLConnection <- function(con) {
  dialect_postgres()
}

#' @export
db_connection_describe.PostgreSQLConnection <- db_connection_describe.PqConnection

#' @export
db_col_types.PostgreSQLConnection <- db_col_types.PqConnection
# nocov end
