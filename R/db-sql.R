#' SQL generation generics
#'
#' @description
#' * `sql_table_analyze(con, table)` generates SQL that "analyzes" the table,
#'   ensuring that the database has up-to-date statistics for use in the query
#'   planner. It called from [copy_to()] when `analyze = TRUE`.
#'
#' * `sql_query_explain(con, sql)` generates SQL that "explains" a query,
#'   i.e. generates a query plan describing what indexes etc that the
#'   database will use.
#'
#' * `sql_index_create()` generates SQL for adding an index to table.
#'
#' * `sql_query_fields()` generates SQL for a 0-row result that is used to
#'   capture field names in [tbl_sql()]
#'
#' * `sql_expr_matches(con, x, y)` is used to generate an alternative to
#'   `x == y` to use when you want `NULL`s to match. The default translation
#'   uses a `CASE WHEN` as described in
#'   <https://modern-sql.com/feature/is-distinct-from>
#'
#' @section dbplyr 2.0.0:
#'
#' These generics replace many of the `db_` generics provided by dplyr. To
#' update your backend, you'll need to extract out the SQL generation from your
#' existing code, and place it in a new method for a dbplyr `sql_` generic.
#'
#' * `db_analyze()` is replaced by `sql_table_analyze()`
#' * `db_explain()` is replaced by `sql_query_explain()`
#' * `db_create_index()` is replaced by `sql_index_create()`
#' * `db_query_fields()` is replaced by `sql_query_fields()`
#' * `db_query_rows()` is no longer used; you can delete it
#'
#' Learn more in `vignette("backend-2.0")`
#'
#' @keywords internal
#' @family generic
#' @name db-sql
NULL

#' @export
sql_subquery.DBIConnection <- function(con, from, name = unique_subquery_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    build_sql("(", from, ") ", ident(name %||% unique_subquery_name()), con = con)
  }
}

#' @rdname db-sql
#' @export
sql_query_explain <- function(con, sql, ...) {
  UseMethod("sql_query_explain")
}
#' @export
sql_query_explain.DBIConnection <- function(con, sql, ...) {
  build_sql("EXPLAIN ", sql, con = con)
}

#' @rdname db-sql
#' @export
sql_table_analyze <- function(con, table, ...) {
  UseMethod("sql_table_analyze")
}
#' @export
sql_table_analyze.DBIConnection <- function(con, table, ...) {
  build_sql("ANALYZE ", as.sql(table), con = con)
}

#' @rdname db-sql
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

#' @rdname db-sql
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

#' @rdname db-sql
#' @export
sql_query_fields <- function(con, sql, ...) {
  UseMethod("sql_query_fields")
}
#' @export
sql_query_fields.DBIConnection <- function(con, sql, ...) {
  sql_select(con, sql("*"), sql_subquery(con, sql), where = sql("0 = 1"))
}

#' @rdname db-sql
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
#' @rdname db-sql
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

# dplyr fallbacks ---------------------------------------------------------

dbplyr_fallback <- function(con, .generic, ...) {
  if (dbplyr_edition(con) >= 2) {
    # Always call DBIConnection method which contains the default implementation
    fun <- sym(paste0(.generic, ".DBIConnection"))
  } else {
    fun <- call("::", quote(dplyr), sym(.generic))
  }
  eval_bare(expr((!!fun)(con, ...)))
}

dbplyr_analyze <- function(con, ...) {
  dbplyr_fallback(con, "db_analyze", ...)
}
#' @export
#' @importFrom dplyr db_analyze
#' @rdname db-sql
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- sql_table_analyze(con, table, ...)
  if (is.null(sql)) {
    return()
  }
  dbExecute(con, sql)
}

dbplyr_create_index <- function(con, ...) {
  dbplyr_fallback(con, "db_create_index", ...)
}
#' @export
#' @rdname db-sql
#' @importFrom dplyr db_create_index
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                          unique = FALSE, ...) {
  sql <- sql_index_create(con, table, columns, name = name, unique = unique, ...)
  dbExecute(con, sql)
}

dbplyr_explain <- function(con, ...) {
  dbplyr_fallback(con, "db_explain", ...)
}
#' @export
#' @importFrom dplyr db_explain
#' @rdname db-sql
db_explain.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_explain(con, sql, ...)
  expl <- dbGetQuery(con, sql)
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

dbplyr_query_fields <- function(con, ...) {
  dbplyr_fallback(con, "db_query_fields", ...)
}
#' @export
#' @rdname db-sql
#' @importFrom dplyr db_query_fields
db_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_fields(con, sql, ...)
  names(dbGetQuery(con, sql))
}
