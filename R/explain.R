#' @importFrom dplyr show_query
#' @export
#' @rdname show_sql
show_query.tbl_lazy <- function(x, ..., cte = FALSE, sql_options = NULL) {
  withr::local_options(list(dbplyr_use_colour = TRUE))
  sql <- remote_query(x, cte = cte, sql_options = sql_options)
  cat_line("<SQL>")
  cat_line(sql)
  invisible(x)
}

#' @importFrom dplyr explain
#' @export
#' @describeIn show_sql Combine the `show_query()` and `remote_query_plan()`
#'   summaries of `x`.
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)
  cat_line()
  cat_line("<PLAN>")
  cat_line(remote_query_plan(x, ...))

  invisible(x)
}

#' Show the query backing a tbl
#'
#' These functions all break the abstraction layer and directly display
#'   the underlying query that will be dispatched to the SQL engine
#'   backing a connection.
#'
#' @inheritParams remote_query
#' @param x An object to explain
#' @param ... For `explain()`, further arguments to [remote_query_plan()].
#'   For `show_sql()`, further arguments to other methods. For `show_query()`,
#'   ignored.
#' @export
show_sql <- function(x, ...) {
  UseMethod("show_sql")
}

#' @export
show_sql.tbl_lazy <- show_query.tbl_lazy
