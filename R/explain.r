#' @export
show_query.tbl_sql <- function(x, ...) {
  message("<SQL>\n", remote_query(x))
  invisible(x)
}

#' @export
show_query.tbl_lazy <- function(x, ...) {
  qry <- sql_build(x, con = x$src, ...)
  sql_render(qry, con = x$src, ...)
}

#' @export
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)

  message("\n")
  message("<PLAN>\n", remote_query_plan(x))

  invisible(x)
}

