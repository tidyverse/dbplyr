
#' @export
sql_translate_env.PostgreSQL <- function(con) {
  sql_translate_env.PostgreSQLConnection(con)
}

# DBI methods ------------------------------------------------------------------

# Doesn't return TRUE for temporary tables
#' @export
db_has_table.PostgreSQL <- function(con, table, ...) {
  db_has_table.PostgreSQLConnection(con, table, ...)
}

#' @export
db_begin.PostgreSQL <- function(con, ...) {
  db_begin.PostgreSQLConnection(con, ...)
}

# http://www.postgresql.org/docs/9.3/static/sql-explain.html
#' @export
db_explain.PostgreSQL <- function(con, sql, format = "text", ...) {
  db_explain.PostgreSQLConnection(con, sql, format = format, ...)
}

#' @export
db_write_table.PostgreSQL <- function(con, table, types, values,
                                      temporary = TRUE, ...) {

  db_write_table.PostgreSQLConnection(con, table, types, values,
                                      temporary = temporary, ...)
}

#' @export
db_query_fields.PostgreSQL <- function(con, sql, ...) {
  db_query_fields.PostgreSQLConnection(con, sql, ...)
}
