#' @importFrom dplyr show_query
#' @export
show_query.tbl_lazy <- function(x, ..., cte = FALSE) {
  sql <- remote_query(x, cte = cte)
  # TODO is this still needed?
  # unique_subquery_name_reset()
  cat_line("<SQL>")
  cat_line(sql)
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
