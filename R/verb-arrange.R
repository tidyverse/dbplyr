#' Arrange rows by variables in a remote database table
#'
#' Order rows of database tables by an expression involving its variables.
#'
#' @section Missing values:
#' Compared to its sorting behavior on local data, the [arrange()] method for
#' most database tables sorts NA at the beginning unless wrapped with [desc()].
#' Users can override this behaviour by explicitly sorting on `is.na(x)`.
#'
#' @inheritParams dplyr::arrange
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
arrange.tbl_lazy <- function(.data, ..., .by_group = FALSE) {
  dots <- quos(...)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_single(
    "arrange",
    .data,
    dots = dots,
    args = list(.by_group = .by_group)
  )
}

#' @export
op_sort.op_arrange <- function(op) {
  c(op_sort(op$x), op$dots)
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
