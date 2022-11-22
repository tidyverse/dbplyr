#' Subset rows using their positions
#'
#' @description
#' These are methods for the dplyr generics [slice_min()], [slice_max()], and
#' [slice_sample()]. They are translated to SQL using [filter()] and
#' window functions (`ROWNUMBER`, `MIN_RANK`, or `CUME_DIST` depending on
#' arguments). `slice()`, `slice_head()`, and `slice_tail()` are not supported
#' since database tables have no intrinsic order.
#'
#' If data is grouped, the operation will be performed on each group so that
#' (e.g.) `slice_min(db, x, n = 3)` will select the three rows with the smallest
#' value of `x` in each group.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::slice
#' @param ... Not used.
#' @param n,prop Provide either `n`, the number of rows, or `prop`, the
#'   proportion of rows to select. If neither are supplied, `n = 1` will be
#'   used.
#'
#'   If `n` is greater than the number of rows in the group (or `prop` > 1),
#'   the result will be silently truncated to the group size. If the proportion
#'   of a group size is not an integer, it is rounded down.
#' @param order_by Variable or function of variables to order by.
#' @param with_ties Should ties be kept together? The default, `TRUE`, may
#'   return more rows than you request. Use FALSE to ignore ties, and return
#'   the first n rows.
#' @param weight_by,replace Not supported for database backends.
#' @name dbplyr-slice
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = 1:3, y = c(1, 1, 2))
#' db %>% slice_min(x) %>% show_query()
#' db %>% slice_max(x) %>% show_query()
#' db %>% slice_sample() %>% show_query()
#'
#' db %>% group_by(y) %>% slice_min(x) %>% show_query()
#'
#' # By default, ties are includes so you may get more rows
#' # than you expect
#' db %>% slice_min(y, n = 1)
#' db %>% slice_min(y, n = 1, with_ties = FALSE)
#'
#' # Non-integer group sizes are rounded down
#' db %>% slice_min(x, prop = 0.5)
NULL

#' @importFrom dplyr slice
#' @export
slice.tbl_lazy <- function(.data, ...) {
  cli_abort("{.fun slice} is not supported on database backends")
}

#' @importFrom dplyr slice_head
#' @export
slice_head.tbl_lazy <- function(.data, ..., n, prop, by = NULL) {
  cli_abort(c(
    "{.fun slice_head} is not supported on database backends",
    i = "Please use {.fun slice_min} instead"
  ))
}

#' @importFrom dplyr slice_tail
#' @export
slice_tail.tbl_lazy <- function(.data, ..., n, prop, by = NULL) {
  cli_abort(c(
    "{.fun slice_tail} is not supported on database backends",
    i = "Please use {.fun slice_max} instead"
  ))
}

#' @rdname dbplyr-slice
#' @importFrom dplyr slice_min
#' @export
slice_min.tbl_lazy <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE) {
  if (missing(order_by)) {
    cli_abort("Argument {.arg order_by} is missing, with no default.")
  }
  size <- check_slice_size(n, prop)
  slice_by(.data, {{order_by}}, size, {{ by }}, with_ties = with_ties)
}

#' @rdname dbplyr-slice
#' @importFrom dplyr slice_max
#' @export
slice_max.tbl_lazy <- function(.data, order_by, ..., n, by = NULL, prop, with_ties = TRUE) {
  if (missing(order_by)) {
    cli_abort("Argument {.arg order_by} is missing, with no default.")
  }
  size <- check_slice_size(n, prop)

  slice_by(.data, desc({{order_by}}), size, {{ by }}, with_ties = with_ties)
}

#' @rdname dbplyr-slice
#' @importFrom dplyr slice_sample
#' @export
slice_sample.tbl_lazy <- function(.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE) {
  size <- check_slice_size(n, prop)
  weight_by <- enquo(weight_by)
  if (size$type == "prop") {
    cli_abort("Sampling by {.arg prop} is not supported on database backends")
  }

  if (!quo_is_null(weight_by)) {
    cli_abort("Weighted resampling is not supported on database backends")
  }
  if (replace) {
    cli_abort("Sampling with replacement is not supported on database backends")
  }

  slice_by(.data, !!sql_random(remote_con(.data)), size, {{ by }}, with_ties = FALSE)
}

slice_by <- function(.data, order_by, size, .by, with_ties = FALSE) {
  by <- compute_by(
    {{ .by }},
    .data,
    by_arg = "by",
    data_arg = "data",
    error_call = caller_env()
  )
  if (by$from_by) {
    .data$lazy_query$group_vars <- by$names
  }

  old_frame <- op_sort(.data)

  if (with_ties) {
    window_fun <- switch(size$type,
      n = expr(min_rank() <= !!size$n),
      prop = expr(cume_dist() <= !!size$prop)
    )
  } else {
    window_fun <- switch(size$type,
      n = expr(row_number() <= !!size$n),
      prop = cli_abort("Can only use {.arg prop} when {.code with_ties = TRUE}")
    )
  }

  out <- .data %>%
    window_order({{order_by}}) %>%
    filter(!!window_fun) %>%
    window_order(!!!old_frame)

  if (by$from_by) {
    out$lazy_query$group_vars <- character()
  }

  out
}


# helpers -----------------------------------------------------------------

check_slice_size <- function(n, prop) {
  if (missing(n) && missing(prop)) {
    list(type = "n", n = 1L)
  } else if (!missing(n) && missing(prop)) {
    if (!is.numeric(n) || length(n) != 1) {
      cli_abort("{.arg n} must be a single number.", call = caller_env())
    }
    if (is.na(n) || n < 0) {
      cli_abort("{.arg n} must be a non-missing positive number.", call = caller_env())
    }

    list(type = "n", n = as.integer(n))
  } else if (!missing(prop) && missing(n)) {
    if (!is.numeric(prop) || length(prop) != 1) {
      cli_abort("{.arg prop} must be a single number", call = caller_env())
    }
    if (is.na(prop) || prop < 0) {
      cli_abort("{.arg prop} must be a non-missing positive number.", call = caller_env())
    }
    list(type = "prop", prop = prop)
  } else {
    cli_abort("Must supply exactly one of {.arg n} and {.arg prop} arguments.", call = caller_env())
  }
}

globalVariables(c("min_rank", "cume_dist", "row_number", "desc"))
