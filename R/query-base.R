#' Create a base lazy query
#'
#' `lazy_base_query()` is a constructor for base lazy query objects. A base
#' lazy query represents the root of a lazy query tree, i.e. a database table or
#' query.
#'
#' @param x A data source, typically a table identifier created by [in_schema()]
#'   or [in_catalog()], or a literal [sql()] string.
#' @param vars A character vector of column names.
#' @param class A character vector of additional subclasses to add. The
#'   resulting object will have class `c("lazy_base_{class}_query",
#'   "lazy_base_query", "lazy_query")`.
#' @param ... Additional arguments passed to [lazy_query()].
#' @returns A lazy query object.
#' @keywords internal
#' @export
lazy_base_query <- function(x, vars, class = character(), ...) {
  check_character(vars)

  lazy_query(
    query_type = c(paste0("base_", class), "base"),
    x = x,
    vars = vars,
    ...,
    group_vars = character(),
    order_vars = NULL,
    frame = NULL
  )
}

lazy_query_remote <- function(x, vars) {
  lazy_base_query(x, vars, class = "remote")
}

#' @export
sql_build.lazy_base_remote_query <- function(op, con, ...) {
  base_query(op$x)
}

#' @export
sql_build.lazy_base_local_query <- function(op, con, ...) {
  base_query(op$name)
}

# base_query() ----------------------------------------------------------------

base_query <- function(from) {
  check_table_source(from)
  query("base", from = from)
}

#' @export
sql_render.base_query <- function(
  query,
  con = NULL,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  from <- query$from
  if (subquery || is.sql(from)) {
    from
  } else {
    from <- sql_escape_table_source(con, from)
    sql_query_select(con, sql("*"), from, lvl = lvl)
  }
}

#' @export
flatten_query.base_query <- function(qry, query_list, con) {
  query_list
}
