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
  dots <- partial_eval(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_order(.data, dots)
}

#' @export
#' @rdname window_order
window_frame <- function(.data, from = -Inf, to = Inf) {
  stopifnot(is.numeric(from), length(from) == 1)
  stopifnot(is.numeric(to), length(to) == 1)

  add_op_single("frame", .data, args = list(range = c(from, to)))
}

#' @export
sql_build.op_frame <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}

#' @export
#' @rdname lazy_ops
op_frame <- function(op) UseMethod("op_frame")

#' @export
op_frame.tbl_lazy <- function(op) {
  op_frame(op$ops)
}

#' @export
op_frame.op_base <- function(op) {
  NULL
}
#' @export
op_frame.op_single <- function(op) {
  op_frame(op$x)
}

#' @export
op_frame.op_double <- function(op) {
  op_frame(op$x)
}

#' @export
op_frame.op_frame <- function(op) {
  op$args$range
}
