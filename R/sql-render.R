#' @export
#' @rdname sql_build
sql_render <- function(query, con = NULL, ...) {
  UseMethod("sql_render")
}

#' @export
sql_render.tbl_lazy <- function(query, con = query$src, ...) {
  # only used for testing
  qry <- sql_build(query$ops, con = con, ...)
  sql_render(qry, con = con, ...)
}

#' @export
sql_render.tbl_sql <- function(query, con = query$src$con, ...) {
  # only used for testing
  qry <- sql_build(query$ops, con = con, ...)
  sql_render(qry, con = con, ...)
}

#' @export
sql_render.op <- function(query, con = NULL, ...) {
  sql_render(sql_build(query, con = con, ...), con = con, ...)
}

#' @export
sql_render.ident <- function(query, con = NULL, ..., root = TRUE) {
  if (root) {
    sql_select(con, sql("*"), query)
  } else {
    query
  }
}

#' @export
sql_render.sql <- function(query, con = NULL, ...) {
  query
}
