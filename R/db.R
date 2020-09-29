#' Miscellaneous database generics
#'
#' @family generic
#' @name db-misc
#' @aliases NULL
NULL

#' @rdname db-misc
#' @export
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
