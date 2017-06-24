#' @export
db_desc.DBIConnection <- function(x) {
  class(x)[[1]]
}

#' @export
db_list_tables.DBIConnection <- function(con) dbListTables(con)

#' @export
db_has_table.DBIConnection <- function(con, table) dbExistsTable(con, table)

#' @export
db_data_type.DBIConnection <- function(con, fields) {
  vapply(fields, dbDataType, dbObj = con, FUN.VALUE = character(1))
}

#' @export
db_save_query.DBIConnection <- function(con, sql, name, temporary = TRUE,
                                        ...) {
  tt_sql <- build_sql(
    "CREATE ", if (temporary) sql("TEMPORARY "),
    "TABLE ", as.sql(name), " AS ", sql,
    con = con
  )
  dbExecute(con, tt_sql)
  name
}

#' @export
db_begin.DBIConnection <- function(con, ...) {
  dbBegin(con)
}

#' @export
db_commit.DBIConnection <- function(con, ...) dbCommit(con)

#' @export
db_rollback.DBIConnection <- function(con, ...) dbRollback(con)

#' @export
db_write_table.DBIConnection <- function(con, table, types, values, temporary = FALSE, ...) {
  dbWriteTable(
    con,
    name = dbi_quote(as.sql(table), con),
    value = values,
    field.types = types,
    temporary = temporary,
    row.names = FALSE
  )
}

#' @export
db_create_table.DBIConnection <- function(con, table, types,
                                          temporary = FALSE, ...) {
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

  dbExecute(con, sql)
}

#' @export
db_insert_into.DBIConnection <- function(con, table, values, ...) {
  dbWriteTable(con, table, values, append = TRUE, row.names = FALSE)
}

#' @export
db_create_indexes.DBIConnection <- function(con, table, indexes = NULL,
                                            unique = FALSE, ...) {
  if (is.null(indexes)) return()
  assert_that(is.list(indexes))

  for (index in indexes) {
    db_create_index(con, table, index, unique = unique, ...)
  }
}

#' @export
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                          unique = FALSE, ...) {
  assert_that(is_string(table), is.character(columns))

  name <- name %||% paste0(c(table, columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  sql <- build_sql(
    "CREATE ", if (unique) sql("UNIQUE "), "INDEX ", as.sql(name),
    " ON ", as.sql(table), " ", fields,
    con = con)

  dbExecute(con, sql)
}

#' @export
db_drop_table.DBIConnection <- function(con, table, force = FALSE, ...) {
  sql <- build_sql(
    "DROP TABLE ", if (force) sql("IF EXISTS "), as.sql(table),
    con = con
  )
  dbExecute(con, sql)
}

#' @export
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- build_sql("ANALYZE ", as.sql(table), con = con)
  dbExecute(con, sql)
}

#' @export
db_explain.DBIConnection <- function(con, sql, ...) {
  exsql <- build_sql("EXPLAIN ", sql, con = con)
  expl <- dbGetQuery(con, exsql)
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
  rows <- build_sql("SELECT count(*) FROM ", from, con = con)

  as.integer(dbGetQuery(con, rows)[[1]])
}

# Utility functions ------------------------------------------------------------

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

# Creates an environment that disconnects the database when it's
# garbage collected
db_disconnector <- function(con, name, quiet = FALSE) {
  reg.finalizer(environment(), function(...) {
    if (!quiet) {
      message(
        "Auto-disconnecting ", name, " connection ",
        "(", paste(con@Id, collapse = ", "), ")"
      )
    }
    dbDisconnect(con)
  })
  environment()
}

res_warn_incomplete <- function(res, hint = "n = -1") {
  if (dbHasCompleted(res)) return()

  rows <- big_mark(dbGetRowCount(res))
  warning("Only first ", rows, " results retrieved. Use ", hint, " to retrieve all.",
    call. = FALSE)
}


dbi_quote <- function(x, con) UseMethod("dbi_quote")
dbi_quote.ident_q <- function(x, con) DBI::SQL(x)
dbi_quote.ident <- function(x, con) DBI::dbQuoteIdentifier(con, x)
dbi_quote.character <- function(x, con) DBI::dbQuoteString(con, x)
dbi_quote.sql <- function(x, con) DBI::SQL(x)
