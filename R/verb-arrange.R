#' Arrange rows by variables in a local lazy tibble
#'
#' Order tbl_lazy rows by an expression involving its variables.
#'
#' @section Missing values:
#' When applied to `tbl_lazy`, the [arrange()] method sorts NAs first (at the
#' beginning) compared to the default sort behavior on data frames, which places
#' NA values last (at the end).
#'
#' @inheritParams dplyr::arrange
#' @return An object of the same class as `.data`.
#' @examples
#' library(dplyr)
#'
#' dbplyr::memdb_frame(a = c(3, 4, 1, 2)) %>%
#'   arrange(a)
#'
#' # tbl_lazy: NAs sorted first
#' dbplyr::memdb_frame(a = c(3, 4, NA, 2)) %>%
#'   arrange(a)
#'
#' # tbl_df: NAs sorted last
#' tibble(a = c(3, 5, NA, 2)) %>%
#'   arrange(a)
#'
#' @export
arrange.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_single("arrange", .data, dots = dots)
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
  group_vars <- c.sql(ident(op_grps(op$x)), con = con)

  select_query(sql_build(op$x, con), order_by = order_vars)
}
