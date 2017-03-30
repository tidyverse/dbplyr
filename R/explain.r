#' @export
show_query.tbl_sql <- function(x, ...) {
  message("<SQL>\n", sql_render(x, con = x$src$con))

  invisible(x)
}

#' @export
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)

  message("\n")
  message("<PLAN>\n", db_explain(con, sql_render(x, con = x$src$con)))

  invisible(x)
}

