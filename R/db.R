# db_ methods -------------------------------------------------------------

#' @export
db_desc.DBIConnection <- function(x) {
  class(x)[[1]]
}

#' @export
db_data_type.DBIConnection <- function(con, fields) {
  vapply(fields, dbDataType, dbObj = con, FUN.VALUE = character(1))
}

#' @export
db_save_query.DBIConnection <- function(con, sql, name, temporary = TRUE,
                                        ...) {
  sql <- sql_save_query(con, sql, name, temporary = temporary, ...)
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
db_create_table.DBIConnection <- function(con, table, types,
                                          temporary = TRUE, ...) {
  assert_that(is_string(table), is.character(types))

  field_names <- escape(ident(names(types)), collapse = NULL, con = con)
  fields <- sql_vector(
    paste0(field_names, " ", types),
    parens = TRUE,
    collapse = ", ",
    con = con
  )
  sql <- build_sql(
    "CREATE ", if (temporary) sql("TEMPORARY "),
    "TABLE ", as.sql(table), " ", fields,
    con = con
  )

  dbExecute(con, sql, immediate = TRUE)
}

#' @export
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                          unique = FALSE, ...) {
  sql <- sql_create_index(con, table, columns, name = name, unique = unique, ...)
  dbExecute(con, sql)
}

#' @export
db_drop_table.DBIConnection <- function(con, table, force = FALSE, ...) {
  sql <- sql_drop_table(con, table, force = force, ...)
  dbExecute(con, sql)
}

#' @export
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- sql_analyze(con, table, ...)
  if (is.null(sql)) {
    return()
  }
  dbExecute(con, sql)
}

#' @export
db_explain.DBIConnection <- function(con, sql, ...) {
  sql <- sql_explain(con, sql, ...)
  expl <- dbGetQuery(con, sql)
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

#' @export
db_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- sql_select(con, sql("*"), sql_subquery(con, sql), where = sql("0 = 1"))
  qry <- dbSendQuery(con, sql)
  on.exit(dbClearResult(qry))

  res <- dbFetch(qry, 0)
  names(res)
}

#' @export
db_query_rows.DBIConnection <- function(con, sql, ...) {
  from <- sql_subquery(con, sql, "master")
  rows <- build_sql("SELECT COUNT(*) FROM ", from, con = con)

  as.integer(dbGetQuery(con, rows)[[1]])
}

# Utility functions ------------------------------------------------------------

dbi_quote <- function(x, con) UseMethod("dbi_quote")
dbi_quote.ident <- function(x, con) DBI::dbQuoteIdentifier(con, as.character(x))
dbi_quote.character <- function(x, con) DBI::dbQuoteString(con, x)
dbi_quote.sql <- function(x, con) DBI::SQL(as.character(x))
