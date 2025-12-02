#' @include backend-postgres.R
NULL

# nocov start
# Use dbplyr edition 1 for custom method dispatch on RPostgreSQL connections
#' @export
dbplyr_edition.PostgreSQLConnection <- function(con) {
  2L
}

#' @export
db_connection_describe.PostgreSQLConnection <- db_connection_describe.PqConnection

#' @export
sql_translation.PostgreSQLConnection <- sql_translation.PqConnection

#' @export
sql_expr_matches.PostgreSQLConnection <- sql_expr_matches.PqConnection

#' @export
sql_query_explain.PostgreSQLConnection <- sql_query_explain.PqConnection

#' @export
supports_window_clause.PostgreSQLConnection <- function(con) {
  TRUE
}

#' @export
sql_query_insert.PostgreSQLConnection <- sql_query_insert.PostgreSQL

#' @export
sql_query_upsert.PostgreSQLConnection <- sql_query_upsert.PostgreSQL
# nocov end
