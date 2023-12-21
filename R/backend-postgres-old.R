#' @include backend-postgres.R
NULL

# nocov start
# Use dbplyr edition 1 for custom method dispatch on RPostgreSQL connections
#' @export
dbplyr_edition.PostgreSQLConnection <- function(con) {
  1L
}

#' @export
db_write_table.PostgreSQLConnection <- function(con,
                                                table,
                                                types,
                                                values,
                                                temporary = TRUE,
                                                ...) {
  table <- as_table_ident(table)
  if (!isFALSE(temporary)) {
    cli_abort(c(
      "RPostgreSQL backend does not support creation of temporary tables",
      i = "Either set {.code temporary = FALSE} or switch to {.pkg RPostgres}"
    ))
  }

  # RPostgreSQL doesn't handle `Id()` or `SQL()` correctly, so we can only pass
  # the bare table name
  dbWriteTable(
    con,
    name = vctrs::field(table, "table"),
    value = values,
    field.types = types,
    ...,
    row.names = FALSE
  )

  table
}

#' @export
db_query_fields.PostgreSQLConnection <- function(con, sql, ...) {
  sql <- sql_subquery(con, sql)
  fields <- glue_sql2(con, "SELECT * FROM {.from sql} WHERE 0=1")

  qry <- dbSendQuery(con, fields)
  on.exit(dbClearResult(qry))

  dbGetInfo(qry)$fieldDescription[[1]]$name
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
