#' @export
show_query.tbl_sql <- function(x, ...) {
  message("<SQL>\n", db_sql_render(x$src$con, x))
  invisible(x)
}

#' @export
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)

  message("\n")
  message("<PLAN>\n", db_explain(x$con, db_sql_render(x$con, x$ops)))

  invisible(x)
}

