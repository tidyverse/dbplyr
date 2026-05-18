#' Show generated SQL and query plan
#'
#' `show_sql()` displays the SQL query that will be dispatched to the database;
#' `explain()` displays both the SQL query and the query plan.
#'
#' @param ... For `explain()`, further arguments to [remote_query_plan()].
#'   For `show_query()`, ignored.
#' @param x An object to explain
#' @importFrom dplyr show_query
#' @export
#' @param use_colour Should the output be coloured?
#' @inheritParams remote_query
#' @name show_query
show_query.tbl_lazy <- function(
  x,
  ...,
  use_colour = TRUE,
  sql_options = NULL,
  cte = deprecated()
) {
  rlang::check_exclusive(sql_options, cte, .require = FALSE)
  if (lifecycle::is_present(cte)) {
    lifecycle::deprecate_warn(
      when = "2.4.0",
      what = "show_query(cte)",
      with = I("show_query(sql_options = sql_options(cte = TRUE))")
    )
    sql_options <- dbplyr::sql_options(cte = cte)
  }
  withr::local_options(list(dbplyr_use_colour = use_colour))
  sql <- remote_query(x, sql_options = sql_options)
  cat_line("<SQL>")
  cat_line(sql)
  invisible(x)
}

#' @importFrom dplyr explain
#' @export
#' @rdname show_query
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)
  cat_line()
  cat_line("<PLAN>")
  cat_line(remote_query_plan(x, ...))

  invisible(x)
}
