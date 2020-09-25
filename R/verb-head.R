#' Subset the first rows
#'
#' @description
#' This is a method for the [head()] generic. It is usually translated to the
#' `LIMIT` clause of the SQL query. Because `LIMIT` is not an official part of
#' the SQL specification, some database use other clauses like `TOP` or
#' `FETCH ROWS`.
#'
#' Note that databases don't really have a sense of row order, so what "first"
#' means is subject to interpretation. Most databases will respect ordering
#' performed with `arrange()`, but it's not guaranteed. `tail()` is not
#' supported at all because the situation is even murkier for the "last" rows.
#'
#' @param x A lazy data frame backed by a database query.
#' @param n Number of rows to return
#' @param ... Not used.
#' @inherit arrange.tbl_lazy return
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = 1:100)
#' db %>% head() %>% show_query()
#'
#' # Pretend we have data in a SQL server database
#' db2 <- lazy_frame(x = 1:100, con = simulate_mssql())
#' db2 %>% head() %>% show_query()
head.tbl_lazy <- function(x, n = 6L, ...) {
  if (!is.numeric(n) || length(n) != 1L || n < 0) {
    abort("`n` must be a non-negative integer")
  }
  n <- trunc(n)

  if (inherits(x$ops, "op_head")) {
    x$ops$args$n <- min(x$ops$args$n, n)
  } else {
    x$ops <- op_single("head", x = x$ops, args = list(n = n))
  }
  x
}

#' @export
tail.tbl_lazy <- function(x, n = 6L, ...) {
  stop("tail() is not supported by sql sources", call. = FALSE)
}

#' @export
sql_build.op_head <- function(op, con, ...) {
  select_query(sql_build(op$x, con), limit = op$args$n)
}

