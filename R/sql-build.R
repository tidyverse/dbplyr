#' Build and render SQL from a sequence of lazy operations
#'
#' `sql_build()` creates a `select_query` S3 object, that is rendered
#' to a SQL string by `sql_render()`. The output from `sql_build()` is
#' designed to be easy to test, as it's database agnostic, and has
#' a hierarchical structure.
#'
#' `sql_build()` is generic over the lazy operations, \link{lazy_ops},
#' and generates an S3 object that represents the query. `sql_render()`
#' takes a query object and then calls a function that is generic
#' over the database. For example, `sql_build.op_mutate()` generates
#' a `select_query`, and `sql_render.select_query()` calls
#' `sql_select()`, which has different methods for different databases.
#' The default methods should generate ANSI 92 SQL where possible, so you
#' backends only need to override the methods if the backend is not ANSI
#' compliant.
#'
#' @export
#' @keywords internal
#' @param op A sequence of lazy operations
#' @param con A database connection. The default `NULL` uses a set of
#'   rules that should be very similar to ANSI 92, and allows for testing
#'   without an active database connection.
#' @param ... Other arguments passed on to the methods. Not currently used.
sql_build <- function(op, con = NULL, ...) {
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_lazy <- function(op, con = op$src$con %||% op$src, ...) {
  # only used for testing
  qry <- sql_build(op$ops, con = con, ...)
  sql_optimise(qry, con = con, ...)
}

# Base ops --------------------------------------------------------

#' @export
sql_build.op_base_remote <- function(op, con, ...) {
  op$x
}

#' @export
sql_build.op_base_local <- function(op, con, ...) {
  ident("df")
}
