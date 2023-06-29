#' Arrange rows by column values
#'
#' @description
#' This is an method for the dplyr [arrange()] generic. It generates
#' the `ORDER BY` clause of the SQL query. It also affects the
#' [window_order()] of windowed expressions in [mutate.tbl_lazy()].
#'
#' Note that `ORDER BY` clauses can not generally appear in subqueries, which
#' means that you should `arrange()` as late as possible in your pipelines.
#'
#' @section Missing values:
#' Unlike R, most databases sorts `NA` (`NULL`s) at the front. You can
#' can override this behaviour by explicitly sorting on `is.na(x)`.
#'
#' @param .data A lazy data frame backed by a database query.
#' @inheritParams dplyr::arrange
#' @return Another `tbl_lazy`. Use [show_query()] to see the generated
#'   query, and use [`collect()`][collect.tbl_sql] to execute the query
#'   and return data to R.
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
#' db %>% arrange(a) %>% show_query()
#'
#' # Note that NAs are sorted first
#' db %>% arrange(b)
#' # override by sorting on is.na() first
#' db %>% arrange(is.na(b), b)
#' @export
#' @importFrom dplyr arrange
arrange.tbl_lazy <- function(.data, ..., .by_group = FALSE) {
  dots <- partial_eval_dots(.data, ..., .named = FALSE)
  names(dots) <- NULL

  .data$lazy_query <- add_arrange(.data, dots, .by_group)
  .data
}

add_arrange <- function(.data, dots, .by_group) {
  lazy_query <- .data$lazy_query

  if (.by_group) {
    dots <- c(syms(op_grps(lazy_query)), dots)
  }
  if (identical(dots, lazy_query$order_vars)) {
    return(lazy_query)
  }

  # `dots` must be an empty list so that `arrange()` removes the `order_vars`
  dots <- dots %||% list()

  new_lazy_query <- lazy_select_query(
    x = lazy_query,
    order_by = dots,
    order_vars = dots
  )

  if (!is_lazy_select_query(lazy_query)) {
    return(new_lazy_query)
  }

  # Needed because `ORDER BY` is evaluated before `LIMIT`
  if (!is.null(lazy_query$limit)) {
    return(new_lazy_query)
  }

  lazy_query$order_vars <- dots
  lazy_query$order_by <- dots
  lazy_query
}
