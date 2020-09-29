#' SQL generation generics
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

#' @export
#' @importFrom dplyr db_explain
db_explain.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_explain(con, sql, ...)
  expl <- dbGetQuery(con, sql)
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
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

#' @export
#' @importFrom dplyr db_analyze
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- sql_table_analyze(con, table, ...)
  if (is.null(sql)) {
    return()
  }
  dbExecute(con, sql)
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

#' @export
#' @importFrom dplyr db_create_index
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                          unique = FALSE, ...) {
  sql <- sql_index_create(con, table, columns, name = name, unique = unique, ...)
  dbExecute(con, sql)
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

#' @export
#' @importFrom dplyr db_query_fields
db_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_fields(con, sql, ...)
  names(dbGetQuery(con, sql))
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

#' @export
#' @importFrom dplyr db_query_rows
db_query_rows.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_rows(con, sql, ...)
  as.integer(dbGetQuery(con, rows)[[1]])
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
