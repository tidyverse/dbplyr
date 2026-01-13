#' Database I/O generics
#'
#' @description
#' These generics are responsible for getting data into and out of the
#' database. They should be used a last resort - only use them when you can't
#' make a backend work by providing methods for DBI generics, or for dbplyr's
#' SQL generation generics. They tend to be most needed when a backend has
#' special handling of temporary tables.
#'
#' * `db_copy_to()` implements [copy_to.src_sql()] by calling
#'   `db_write_table()` (which calls [DBI::dbWriteTable()]) to transfer the
#'   data, then optionally adds indexes (via [sql_table_index()]) and
#'   analyses (via [sql_table_analyze()]).
#'
#' * `db_compute()` implements [compute.tbl_sql()] by calling
#'   [sql_query_save()] to create the table, then optionally adds indexes
#'   (via [sql_table_index()]) and analyses (via [sql_table_analyze()]).
#'
#' * `db_collect()` implements [collect.tbl_sql()] using [DBI::dbSendQuery()]
#'   and [DBI::dbFetch()].
#'
#' * `sql_table_temporary()` is used for databases that have special naming
#'   schemes for temporary tables (e.g. SQL server and SAP HANA require
#'   temporary tables to start with `#`)
#'
#' * `db_table_drop_if_exists()` is used to drop a table if it exists. This
#'   is used when `overwrite = TRUE` in [copy_to()] and [compute()].
#'
#' @keywords internal
#' @family generic
#' @name db-io
#' @aliases NULL
NULL

#' @export
#' @rdname db-io
db_copy_to <- function(
  con,
  table,
  values,
  ...,
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  in_transaction = TRUE
) {
  check_table_id(table)
  check_bool(overwrite)
  check_character(types, allow_null = TRUE)
  check_named(types)
  check_bool(temporary)
  check_bool(analyze)
  check_dots_used()
  check_bool(in_transaction)

  UseMethod("db_copy_to")
}
#' @export
db_copy_to.DBIConnection <- function(
  con,
  table,
  values,
  ...,
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  in_transaction = TRUE
) {
  table <- as_table_path(table, con)
  new <- sql_table_temporary(con, table, temporary)
  table <- new$table
  temporary <- new$temporary
  call <- current_env()

  with_transaction(
    con,
    in_transaction,
    "Can't copy data to table {.field {format(table, con = con)}}.",
    {
      table <- dbplyr_write_table(
        con,
        table,
        types = types,
        values = values,
        temporary = temporary,
        overwrite = overwrite,
        ...
      )
      create_indexes(con, table, unique_indexes, unique = TRUE)
      create_indexes(con, table, indexes)
      if (analyze) dbplyr_analyze(con, table)
    }
  )

  table
}

#' @export
#' @rdname db-io
db_compute <- function(
  con,
  table,
  sql,
  ...,
  overwrite = FALSE,
  temporary = TRUE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  in_transaction = TRUE
) {
  check_table_id(table)
  check_scalar_sql(sql)
  check_bool(overwrite)
  check_bool(temporary)
  check_bool(in_transaction)
  check_dots_used()

  UseMethod("db_compute")
}
#' @export
db_compute.DBIConnection <- function(
  con,
  table,
  sql,
  ...,
  overwrite = FALSE,
  temporary = TRUE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  in_transaction = FALSE
) {
  table <- as_table_path(table, con)
  new <- sql_table_temporary(con, table, temporary)
  table <- new$table
  temporary <- new$temporary

  with_transaction(
    con,
    in_transaction,
    "Can't copy query to table {.field {format(table, con = con)}}.",
    {
      table <- dbplyr_save_query(
        con,
        sql,
        table,
        ...,
        temporary = temporary,
        overwrite = overwrite
      )
      create_indexes(con, table, unique_indexes, unique = TRUE)
      create_indexes(con, table, indexes)
      if (analyze) dbplyr_analyze(con, table)
    }
  )

  table
}

#' @export
#' @rdname db-io
db_collect <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  check_dots_used()
  UseMethod("db_collect")
}
#' @export
db_collect.DBIConnection <- function(
  con,
  sql,
  n = -1,
  warn_incomplete = TRUE,
  ...
) {
  res <- DBI::dbSendQuery(con, sql)
  on.exit(DBI::dbClearResult(res), add = TRUE)

  out <- DBI::dbFetch(res, n = n)
  if (warn_incomplete) {
    res_warn_incomplete(res, "n = Inf")
  }

  out
}


dbplyr_write_table <- function(
  con,
  table,
  types,
  values,
  temporary = TRUE,
  ...,
  overwrite = FALSE
) {
  check_table_path(table)
  check_character(types, allow_null = TRUE)
  check_named(types)
  check_bool(temporary)
  check_bool(overwrite)

  UseMethod("dbplyr_write_table")
}

#' @export
dbplyr_write_table.DBIConnection <- function(
  con,
  table,
  types,
  values,
  temporary = TRUE,
  ...,
  overwrite = FALSE
) {
  if (inherits(con, "PostgreSQLConnection")) {
    # RPostgreSQL doesn't handle `Id()` or `SQL()` correctly, so we can only pass
    # the bare table name
    name <- table_path_name(table, con)
  } else {
    name <- DBI::SQL(table)
  }

  withCallingHandlers(
    DBI::dbWriteTable(
      con,
      name = name,
      value = values,
      field.types = types,
      temporary = temporary,
      overwrite = overwrite,
      ...,
      row.names = FALSE
    ),
    error = function(cnd) {
      msg <- "Can't write table table {.field {format(table, con = con)}}."
      cli_abort(msg, parent = cnd)
    }
  )

  table
}

# Utility functions ------------------------------------------------------------

create_indexes <- function(con, table, indexes = NULL, unique = FALSE, ...) {
  if (is.null(indexes)) {
    return()
  }

  indexes <- as.list(indexes)
  for (index in indexes) {
    dbplyr_create_index(con, table, index, unique = unique, ...)
  }
}

with_transaction <- function(
  con,
  in_transaction,
  msg,
  code,
  call = caller_env(),
  env = caller_env()
) {
  if (in_transaction) {
    DBI::dbBegin(con)
    on.exit(DBI::dbRollback(con))
  }

  withCallingHandlers(
    code,
    error = function(cnd) {
      cli_abort(msg, parent = cnd, call = call, .envir = env)
    }
  )

  if (in_transaction) {
    on.exit()
    DBI::dbCommit(con)
  }
}

#' @export
#' @rdname db-io
sql_table_temporary <- function(con, table, temporary, ...) {
  dialect <- sql_dialect(con)
  return(sql_table_temporary_(dialect, table, temporary, ...))

  UseMethod("sql_table_temporary")
}
sql_table_temporary_ <- function(con, table, temporary, ...) {
  UseMethod("sql_table_temporary")
}
#' @export
sql_table_temporary.DBIConnection <- function(con, table, temporary, ...) {
  list(
    table = table,
    temporary = temporary
  )
}
#' @export
sql_table_temporary.sql_dialect <- sql_table_temporary.DBIConnection

#' @rdname db-io
#' @export
db_table_drop_if_exists <- function(con, table, ...) {
  UseMethod("db_table_drop_if_exists")
}

#' @export
db_table_drop_if_exists.DBIConnection <- function(con, table, ...) {
  if (DBI::dbExistsTable(con, DBI::SQL(table))) {
    DBI::dbRemoveTable(con, DBI::SQL(table))
  }
}
