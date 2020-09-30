#' Miscellaneous database generics
#'
#' Use a `dbplyr_edition()` method to declare that your backend supports
#' version 2.0.0 of the dbplyr API where all generics live in dbplyr.
#' See `vignette("backend-2")` for more details.
#'
#' @family generic
#' @keywords internal
#' @name db-misc
#' @aliases NULL
NULL

#' @rdname db-misc
#' @export
#' @importFrom dplyr db_desc
db_desc.DBIConnection <- function(x) {
  class(x)[[1]]
}

#' @rdname db-misc
#' @export
sql_join_suffix <- function(con, ...) {
  UseMethod("sql_join_suffix")
}
#' @export
sql_join_suffix.DBIConnection <- function(con, ...) {
  c(".x", ".y")
}

#' @rdname db-misc
#' @export
db_sql_render <- function(con, sql, ...) {
  UseMethod("db_sql_render")
}
#' @export
db_sql_render.DBIConnection <- function(con, sql, ...) {
  sql_render(sql, con = con, ...)
}

#' @rdname db-misc
#' @export
dbplyr_edition <- function(con) {
  UseMethod("dbplyr_edition")
}
#' @export
dbplyr_edition.DBIConnection <- function(con) {
  1L
}
