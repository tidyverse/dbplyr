# nocov start

#' Create a "sql src" object
#'
#' Deprecated: please use directly use a `DBIConnection` object instead.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass. "src_sql" is an abstract base class, so you
#'   must supply this value. `src_` is automatically prepended to the
#'   class name
#' @param con the connection object
#' @param ... fields used by object
src_sql <- function(subclass, con, ...) {
  lifecycle::deprecate_stop(
    when = "1.4.0",
    what = "src_sql()",
    always = TRUE
  )
}


#' @importFrom dplyr same_src
#' @export
same_src.src_sql <- function(x, y) {
  if (!inherits(y, "src_sql")) return(FALSE)
  identical(x$con, y$con)
}

#' @importFrom dplyr src_tbls
#' @export
src_tbls.src_sql <- function(x, ...) {
  dbListTables(x$con)
}

#' @export
format.src_sql <- function(x, ...) {
  paste0(
    "src:  ", dbplyr_connection_describe(x$con), "\n",
    wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", "))
  )
}
# nocov end
