#' Arrange rows by variables in a remote database table
#'
#' Order rows of database tables by an expression involving its variables.
#' Use [window_order()] instead to define the sort order for window functions
#' without affecting the resulting order.
#' The order is active only for the next verb.
#'
#' @details
#' Order is mostly irrelevant in relational databases, except for the following
#' cases:
#'
#' - Returning results with [collect()],
#' - Populating a table with [compute()],
#' - Processing the first few rows with [head()],
#' - Evaluating window functions, see [window_order()].
#'
#' In particular, subqueries never need to specify an order,
#' except when the number of rows returned from the subquery is limited.
#' Therefore, `arrange()` should only be used as the last step in a pipe,
#' or immediately before `head()`.
#'
#' An empty `arrange()` call resets the sort order.
#' This may be useful if the lazy table is created outside of your control.
#'
#' @section dbplyr 1.4.3 and earlier:
#' In dbplyr 1.4.3 and earlier, the `arrange()` and `window_order()` verbs
#' incorrectly added columns to the existing order.
#' Pipes with multiple such verbs led to SQL code
#' that was inconsistent with the intent, and sometimes erroneous.
#' The current version warns if multiple calls to `arrange()`
#' or `window_order()` are used in a pipe.
#' If necessary, add an empty `arrange()` or `window_order()` call
#' to silence this warning.
#'
#' @section Missing values:
#' Compared to its sorting behaviour on local data, the [arrange()] method for
#' most database tables sorts NA at the beginning unless wrapped with [desc()].
#' Users can override this behaviour by explicitly sorting on `is.na(x)`.
#'
#' @inheritParams dplyr::arrange
#' @param ... Variables, or functions or variables. Use desc() to sort a
#'   variable in descending order.
#' @return An object of the same class as `.data`.
#' @examples
#' library(dplyr)
#'
#' dbplyr::memdb_frame(a = c(3, 4, 1, 2)) %>%
#'   arrange(a)
#'
#' # NA sorted first
#' dbplyr::memdb_frame(a = c(3, 4, NA, 2)) %>%
#'   arrange(a)
#'
#' # override by sorting on is.na() first
#' dbplyr::memdb_frame(a = c(3, 4, NA, 2)) %>%
#'   arrange(is.na(a), a)
#'
#' @export
#' @importFrom dplyr arrange
arrange.tbl_lazy <- function(.data, ..., .by_group = FALSE) {
  dots <- quos(...)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  names(dots) <- NULL

  # Special case for empty arrange() after existing arrange():
  if (length(dots) == 0 && !.by_group && inherits(.data$ops, "op_arrange")) {
    .data$ops <- .data$ops$x
    return(.data)
  }

  add_op_single(
    "arrange",
    .data,
    dots = dots,
    args = list(.by_group = .by_group)
  )
}

#' @export
op_sort.op_arrange <- function(op) {
  op$dots
}

#' @export
op_desc.op_arrange <- function(x, ...) {
  op_desc(x$x, ...)
}

#' @export
sql_build.op_arrange <- function(op, con, ...) {
  order_vars <- translate_sql_(op$dots, con, context = list(clause = "ORDER"))

  if (op$args$.by_group) {
    order_vars <- c.sql(ident(op_grps(op$x)), order_vars, con = con)
  }

  select_query(
    sql_build(op$x, con),
    order_by = order_vars
  )
}
