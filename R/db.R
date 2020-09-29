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

# SQL methods -------------------------------------------------------------

#' SQL generation methods for database methods
#'
#' * `sql_table_analyze()` <- `db_analyze()` <- `db_copy_to(analyze = TRUE)`
#' * `sql_index_create()` <- `db_create_index()` <- `db_copy_to(indexes = ...)`
#' * `sql_query_explain()` <- `db_explain` <- `explain()`
#' * `sql_query_fields()` <- `db_query_fields()` <- `tbl()`
#' * `sql_query_rows()` <- `db_query_rows()` <- `do()`
#' * `sql_query_save()` <- `db_save_query()` <- `db_compute()` <- `compute()`
#' * `sql_expr_matches(con, x, y)` is used to generate an alternative to
#'   `x == y` to use when you want `NULL`s to match. The default translation
#'   uses a `CASE WHEN` as described in
#'   <https://modern-sql.com/feature/is-distinct-from>
#'
#' @keywords internal
#' @name db_sql
NULL

#' @export
sql_subquery.DBIConnection <- function(con, from, name = unique_subquery_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    build_sql("(", from, ") ", ident(name %||% unique_subquery_name()), con = con)
  }
}

#' @rdname db_sql
#' @export
sql_query_explain <- function(con, sql, ...) {
  UseMethod("sql_query_explain")
}
#' @export
sql_query_explain.DBIConnection <- function(con, sql, ...) {
  build_sql("EXPLAIN ", sql, con = con)
}

#' @rdname db_sql
#' @export
sql_table_analyze <- function(con, table, ...) {
  UseMethod("sql_table_analyze")
}
#' @export
sql_table_analyze.DBIConnection <- function(con, table, ...) {
  build_sql("ANALYZE ", as.sql(table), con = con)
}

#' @rdname db_sql
#' @export
sql_index_create <- function(con, table, columns, name = NULL, unique = FALSE, ...) {
  UseMethod("sql_index_create")
}
#' @export
sql_index_create.DBIConnection <- function(con, table, columns, name = NULL,
                                           unique = FALSE, ...) {
  assert_that(is_string(table), is.character(columns))

  name <- name %||% paste0(c(unclass(table), columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  build_sql(
    "CREATE ", if (unique) sql("UNIQUE "), "INDEX ", as.sql(name),
    " ON ", as.sql(table), " ", fields,
    con = con
  )
}

#' @rdname db_sql
#' @export
sql_query_save <- function(con, sql, name, temporary = TRUE, ...) {
  UseMethod("sql_query_save")
}
#' @export
sql_query_save.DBIConnection <- function(con, sql, name, temporary = TRUE, ...) {
  build_sql(
    "CREATE ", if (temporary) sql("TEMPORARY "), "TABLE \n",
    as.sql(name), " AS ", sql,
    con = con
  )
}

#' @rdname db_sql
#' @export
sql_join_suffix <- function(con, ...) {
  UseMethod("sql_join_suffix")
}
#' @export
sql_join_suffix.DBIConnection <- function(con, ...) {
  c(".x", ".y")
}

#' @rdname db_sql
#' @export
sql_query_fields <- function(con, sql, ...) {
  UseMethod("sql_query_fields")
}

#' @export
sql_query_fields.DBIConnection <- function(con, sql, ...) {
  sql_select(con, sql("*"), sql_subquery(con, sql), where = sql("0 = 1"))
}

#' @rdname db_sql
#' @export
sql_query_rows <- function(con, sql, ...) {
  UseMethod("sql_query_rows")
}

#' @export
sql_query_rows.DBIConnection <- function(con, sql, ...) {
  from <- sql_subquery(con, sql, "master")
  build_sql("SELECT COUNT(*) FROM ", from, con = con)
}

#' @export
#' @rdname db_sql
sql_expr_matches <- function(con, x, y) {
  UseMethod("sql_expr_matches")
}

# https://modern-sql.com/feature/is-distinct-from
#' @export
sql_expr_matches.DBIConnection <- function(con, x, y) {
  build_sql(
    "CASE WHEN (", x, " = ", y, ") OR (", x, " IS NULL AND ", y, " IS NULL) ",
    "THEN 0 ",
    "ELSE 1 = 0",
    con = con
  )
}

#' More db generics
#'
#' These are new, so not included in dplyr for backward compatibility
#' purposes.
#'
#' @keywords internal
#' @export
db_copy_to <-  function(con, table, values,
                        overwrite = FALSE, types = NULL, temporary = TRUE,
                        unique_indexes = NULL, indexes = NULL,
                        analyze = TRUE, ...,
                        in_transaction = TRUE) {
  UseMethod("db_copy_to")
}


#' @export
db_copy_to.DBIConnection <- function(con, table, values,
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...,
                            in_transaction = TRUE) {

  with_transaction(con, in_transaction, {
    table <- db_write_table(con, table,
      types = types,
      values = values,
      temporary = temporary,
      overwrite = overwrite
    )
    create_indexes(con, table, unique_indexes, unique = TRUE)
    create_indexes(con, table, indexes, unique = FALSE)
    if (analyze) db_analyze(con, table)
  })

  table
}


#' @export
#' @rdname db_copy_to
db_compute <- function(con,
                      table,
                      sql,
                      temporary = TRUE,
                      unique_indexes = list(),
                      indexes = list(),
                      analyze = TRUE,
                      ...) {
  UseMethod("db_compute")
}


#' @export
db_compute.DBIConnection <- function(con,
                                     table,
                                     sql,
                                     temporary = TRUE,
                                     unique_indexes = list(),
                                     indexes = list(),
                                     analyze = TRUE,
                                     ...) {
  if (!is.list(indexes)) {
    indexes <- as.list(indexes)
  }
  if (!is.list(unique_indexes)) {
    unique_indexes <- as.list(unique_indexes)
  }

  table <- db_save_query(con, sql, table, temporary = temporary)
  create_indexes(con, table, unique_indexes, unique = TRUE)
  create_indexes(con, table, indexes, unique = FALSE)
  if (analyze) db_analyze(con, table)

  table
}

#' @rdname db_copy_to
#' @export
db_sql_render <- function(con, sql, ...) {
  UseMethod("db_sql_render")
}

#' @export
db_sql_render.DBIConnection <- function(con, sql, ...) {
  sql_render(sql, con = con, ...)
}

#' @export
#' @rdname db_copy_to
db_collect <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  UseMethod("db_collect")
}

#' @export
db_collect.DBIConnection <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  res <- dbSendQuery(con, sql)
  tryCatch({
    out <- dbFetch(res, n = n)
    if (warn_incomplete) {
      res_warn_incomplete(res, "n = Inf")
    }
  }, finally = {
    dbClearResult(res)
  })

  out
}

# Utility functions ------------------------------------------------------------

dbi_quote <- function(x, con) UseMethod("dbi_quote")
dbi_quote.ident <- function(x, con) DBI::dbQuoteIdentifier(con, as.character(x))
dbi_quote.character <- function(x, con) DBI::dbQuoteString(con, x)
dbi_quote.sql <- function(x, con) DBI::SQL(as.character(x))

create_indexes <- function(con, table, indexes = NULL, unique = FALSE, ...) {
  if (is.null(indexes)) return()
  assert_that(is.list(indexes))

  for (index in indexes) {
    db_create_index(con, table, index, unique = unique, ...)
  }
}

# Don't use `tryCatch()` because it messes with the callstack
with_transaction <- function(con, in_transaction, code) {
  if (in_transaction) {
    dbBegin(con)
    on.exit(dbRollback(con))
  }

  code

  if (in_transaction) {
    on.exit()
    dbCommit(con)
  }
}

