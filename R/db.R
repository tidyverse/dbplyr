#' Miscellaneous database generics
#'
#' * `db_connection_describe()` provides a short string describing the
#'   database connection, helping users tell which database a table comes
#'   from. It should be a single line, and ideally less than 60 characters wide.
#'
#' * `dbplyr_edition()` declares which version of the dbplyr API you want.
#'    See below for more details.
#'
#' @section dplyr 2.0.0:
#' dplyr 2.0.0 renamed a number of generics so that they could be cleanly moved
#' from dplyr to dbplyr. If you have an existing backend, you'll need to rename
#' the following methods.
#'
#' * `dplyr::db_desc()` -> `dbplyr::db_connection_describe()` (also note that
#'    the argument named changed from `x` to `con`).
#'
#' @family generic
#' @keywords internal
#' @name db-misc
#' @aliases NULL
NULL

dbplyr_connection_describe <- function(con, ...) {
  dbplyr_fallback(con, "db_desc", ...)
}
#' @export
#' @importFrom dplyr db_desc
db_desc.DBIConnection <- function(x) {
  db_connection_describe(x)
}
#' @export
#' @rdname db-misc
db_connection_describe <- function(con) {
  UseMethod("db_connection_describe")
}
#' @export
db_connection_describe.DBIConnection <- function(con) {
  class(con)[[1]]
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
dbplyr_edition.default <- function(con) {
  1L
}
# Needed because pool uses an object of call Pool/R6

# fallback helper ---------------------------------------------------------

dbplyr_fallback <- function(con, .generic, ...) {
  if (dbplyr_edition(con) >= 2) {
    # Always call DBIConnection method which contains the default implementation
    fun <- sym(paste0(.generic, ".DBIConnection"))
  } else {
    fun <- call("::", quote(dplyr), sym(.generic))
  }
  eval_bare(expr((!!fun)(con, ...)))
}
