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
#' db |> arrange(a) |> show_query()
#'
#' # Note that NAs are sorted first
#' db |> arrange(b)
#' # override by sorting on is.na() first
#' db |> arrange(is.na(b), b)
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

  # Empty arrange() preserves existing ordering (like dplyr)
  if (is_empty(dots)) {
    return(lazy_query)
  }

  if (.by_group) {
    dots <- c(syms(op_grps(lazy_query)), dots)
  }

  # Prepend new ordering to existing ordering (like dplyr)
  order_vars <- c(dots, op_sort(lazy_query))

  if (can_inline_arrange(lazy_query)) {
    lazy_query$order_vars <- order_vars
    lazy_query$order_by <- order_vars
    lazy_query
  } else {
    lazy_select_query(
      x = lazy_query,
      order_by = order_vars,
      order_vars = order_vars
    )
  }
}

# arrange() adds/modifies the ORDER BY clause
# * ORDER BY is executed after LIMIT
#   => can't inline if LIMIT is set
can_inline_arrange <- function(lazy_query) {
  if (!is_lazy_select_query(lazy_query)) {
    return(FALSE)
  }

  if (!is.null(lazy_query$limit)) {
    return(FALSE)
  }

  TRUE
}

unwrap_order_expr <- function(order_by, f, error_call = caller_env()) {
  order_by_quo <- quo({{ order_by }})
  order_by_env <- quo_get_env(order_by_quo)
  order_by_expr <- quo_get_expr(order_by_quo)

  if (is.null(order_by_expr)) {
    return()
  }

  if (is_call(order_by_expr, "c")) {
    args <- call_args(order_by_expr)
    tibble_expr <- expr_text(expr(tibble(!!!args)))
    cli_abort(
      c(
        "Can't use `c()` in {.fun {f}}",
        i = "Did you mean to use `{tibble_expr}` instead?"
      ),
      call = error_call
    )
  }

  if (is_call(order_by_expr, c("tibble", "data.frame"))) {
    tibble_args <- call_args(order_by_expr)
    # browser()
    out <- as_quosures(tibble_args, env = order_by_env)
    return(out)
  }

  list(order_by_quo)
}

swap_order_direction <- function(x) {
  is_quo <- is_quosure(x)
  if (is_quo) {
    env <- quo_get_env(x)
    x <- quo_get_expr(x)
  }

  if (is_call(x, "desc", n = 1)) {
    out <- call_args(x)[[1]]
  } else {
    out <- expr(desc(!!x))
  }

  if (is_quo) {
    out <- as_quosure(out, env)
  }

  out
}
