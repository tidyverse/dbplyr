#' Override window order and frame
#'
#' @param .data A remote tibble
#' @param ... Name-value pairs of expressions.
#' @param from,to Bounds of the frame.
#' @export
#' @examples
#' library(dplyr)
#' df <- lazy_frame(g = rep(1:2, each = 5), y = runif(10), z = 1:10)
#'
#' df %>%
#'   window_order(y) %>%
#'   mutate(z = cumsum(y)) %>%
#'   sql_build()
#'
#' df %>%
#'   group_by(g) %>%
#'   window_frame(-3, 0) %>%
#'   window_order(z) %>%
#'   mutate(z = sum(x)) %>%
#'   sql_build()
#' @export
window_order <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_order(.data, dots)
}

# We want to preserve this ordering (for window functions) without
# imposing an additional arrange, so we have a special op_order

add_op_order <- function(.data, dots = list()) {
  if (length(dots) == 0) {
    return(.data)
  }

  .data$ops <- op_single("order", x = .data$ops, dots = dots)
  .data
}
#' @export
op_sort.op_order <- function(op) {
  c(op_sort(op$x), op$dots)
}

#' @export
sql_build.op_order <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}

# Frame -------------------------------------------------------------------

#' @export
#' @rdname window_order
window_frame <- function(.data, from = -Inf, to = Inf) {
  stopifnot(is.numeric(from), length(from) == 1)
  stopifnot(is.numeric(to), length(to) == 1)

  add_op_single("frame", .data, args = list(range = c(from, to)))
}

#' @export
op_frame.op_frame <- function(op) {
  op$args$range
}

#' @export
sql_build.op_frame <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}
