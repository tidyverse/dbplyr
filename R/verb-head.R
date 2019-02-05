#' @export
head.tbl_lazy <- function(x, n = 6L, ...) {
  if (inherits(x$ops, "op_head")) {
    x$ops$args$n <- min(x$ops$args$n, n)
  } else {
    x$ops <- op_single("head", x = x$ops, args = list(n = n))
  }
  x
}

#' @export
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail() is not supported by sql sources", call. = FALSE)
}

#' @export
sql_build.op_head <- function(op, con, ...) {
  select_query(sql_build(op$x, con), limit = op$args$n)
}

