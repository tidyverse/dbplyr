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
#' Additionally, `LIMIT` clauses can not generally appear in subqueries, which
#' means that you should use `head()` as late as possible in your pipelines.
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
#' db |> head() |> show_query()
#'
#' # Pretend we have data in a SQL server database
#' db2 <- lazy_frame(x = 1:100, con = simulate_mssql())
#' db2 |> head() |> show_query()
head.tbl_lazy <- function(x, n = 6L, ...) {
  if (!is.numeric(n) || length(n) != 1L || n < 0) {
    cli_abort("{.arg n} must be a non-negative integer")
  }
  n <- trunc(n)

  x$lazy_query <- add_head(x$lazy_query, n)
  x
}

add_head <- function(lazy_query, n) {
  if (can_inline_head(lazy_query)) {
    lazy_query$limit <- min(lazy_query$limit, n)
    lazy_query
  } else {
    lazy_select_query(x = lazy_query, limit = n)
  }
}

# head() modifies the LIMIT clause
# LIMIT happens last so can always inline
can_inline_head <- function(lazy_query) {
  is_lazy_select_query(lazy_query)
}

#' @export
tail.tbl_lazy <- function(x, n = 6L, ...) {
  stop_unsupported_function("tail")
}
