# db_ methods -------------------------------------------------------------

#' @export
db_desc.DBIConnection <- function(x) {
  class(x)[[1]]
}

#' @export
db_save_query.DBIConnection <- function(con, sql, name, temporary = TRUE,
                                        ...) {
  sql <- sql_query_save(con, sql, name, temporary = temporary, ...)
  dbExecute(con, sql, immediate = TRUE)
  name
}

#' @export
db_write_table.DBIConnection <- function(con, table, types, values, temporary = TRUE, overwrite = FALSE, ...) {

  dbWriteTable(
    con,
    name = dbi_quote(table, con),
    value = values,
    field.types = types,
    temporary = temporary,
    overwrite = overwrite,
    row.names = FALSE
  )

  table
}

#' @export
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                          unique = FALSE, ...) {
  sql <- sql_index_create(con, table, columns, name = name, unique = unique, ...)
  dbExecute(con, sql)
}

#' @export
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- sql_table_analyze(con, table, ...)
  if (is.null(sql)) {
    return()
  }
  dbExecute(con, sql)
}

#' @export
db_explain.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_explain(con, sql, ...)
  expl <- dbGetQuery(con, sql)
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

#' @export
db_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_fields(con, sql, ...)
  names(dbGetQuery(con, sql))
}

#' @export
db_query_rows.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_rows(con, sql, ...)
  as.integer(dbGetQuery(con, rows)[[1]])
}

# Utility functions ------------------------------------------------------------

dbi_quote <- function(x, con) UseMethod("dbi_quote")
dbi_quote.ident <- function(x, con) DBI::dbQuoteIdentifier(con, as.character(x))
dbi_quote.character <- function(x, con) DBI::dbQuoteString(con, x)
dbi_quote.sql <- function(x, con) DBI::SQL(as.character(x))
