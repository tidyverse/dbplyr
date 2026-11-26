# Backward compatibility ---------------------------------------------------------
# These functions are provided for backward compatibility with packages that
# used internal dbplyr functions. They should not be used in new code.

# Used by RPresto

#' Optimise a SQL query
#'
#' No longer used.
#'
#' @export
#' @keywords internal
sql_optimise <- function(x, con = NULL, ..., subquery = FALSE) {
  UseMethod("sql_optimise")
}

# Used by ckanr, implyr, sergeant
#' Create a "sql src" object
#'
#' Deprecated: please use directly use a `DBIConnection` object instead.
#'
#' @param subclass Name of subclass.
#' @param con The connection object.
#' @param ... Other arguments passed on to individual methods.
#' @export
#' @keywords internal
src_sql <- function(subclass, con, ...) {
  lifecycle::deprecate_stop(when = "1.4.0", what = "src_sql()")
}

# Used by dm, etl, starwarsdb
#' @importFrom dplyr src_tbls
#' @export
src_tbls.src_sql <- function(x, ...) {
  lifecycle::deprecate_soft("2.6.0", "src_tbls()")
  DBI::dbListTables(x$con)
}

#' @importFrom dplyr same_src
#' @export
same_src.src_sql <- function(x, y) {
  if (!inherits(y, "src_sql")) {
    return(FALSE)
  }
  identical(x$con, y$con)
}

# dplyr:::print.src calls format, so we need to retain this method
#' @export
format.src_sql <- function(x, ...) {
  paste0(
    "src:  ",
    db_connection_describe(x$con),
    "\n",
    wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", "))
  )
}

# Used by healthdb
#' Escape ANSI characters
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [escape()] instead.
#'
#' @param x Object to escape.
#' @param parens,collapse Controls parens and collapsing. Passed on to
#'   [escape()].
#' @export
#' @keywords internal
escape_ansi <- function(x, parens = NA, collapse = "") {
  lifecycle::deprecate_soft("2.6.0", "escape_ansi()", "escape()")
  escape(x, parens = parens, collapse = collapse, con = simulate_dbi())
}
