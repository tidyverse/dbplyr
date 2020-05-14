#' @importFrom dplyr show_query
#' @export
show_query.tbl_lazy <- function(x, ...) {
  cat_line("<SQL>")
  cat_line(remote_query(x))
  invisible(x)
}

#' @importFrom dplyr explain
#' @export
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)
  cat_line()
  cat_line("<PLAN>")
  cat_line(remote_query_plan(x))

  invisible(x)
}
