#' @export
show_query.tbl_sql <- function(x, ...) {
  cat_line("<SQL>")
  cat_line(remote_query(x))
  invisible(x)
}

#' @export
show_query.tbl_lazy <- function(x, ...) {
  sql_render(x, con = x$src, ...)
}

#' @export
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)
  cat_line()
  cat_line("<PLAN>")
  cat_line(remote_query_plan(x))

  invisible(x)
}
