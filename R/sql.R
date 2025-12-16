#' SQL escaping.
#'
#' These functions are critical when writing functions that translate R
#' functions to sql functions. Typically a conversion function should escape
#' all its inputs and return an sql object.
#'
#' @param ... Character vectors that will be combined into a single SQL vector.
#' @param x Object to check if it is an sql object.
#' @export
sql <- function(...) {
  x <- c_character(...)
  structure(x, class = c("sql", "character"))
}

sql_c <- function(...) {
  sql(paste0(unlist(list(...)), collapse = ""))
}

#' @export
`[.sql` <- function(x, i) {
  sql(NextMethod())
}
#' @export
`[[.sql` <- function(x, i) {
  sql(NextMethod())
}

#' @rdname sql
#' @export
is.sql <- function(x) inherits(x, "sql")

#' @export
print.sql <- function(x, ...) cat(format(x, ...), sep = "\n")
#' @export
format.sql <- function(x, ...) {
  if (length(x) == 0) {
    paste0("<SQL> [empty]")
  } else {
    paste0("<SQL> ", x, ifelse(names2(x) == "", "", paste0(" AS ", names2(x))))
  }
}

#' Convert to sql (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @keywords internal
#'
#' @param x Object to coerce
#' @param con Needed when `x` is directly supplied from the user so that
#'   schema specifications can be quoted using the correct identifiers.
#' @export
as.sql <- function(x, con) {
  lifecycle::deprecate_warn("2.6.0", "as.sql()", "as_table_path()")
  if (is_bare_character(x)) {
    ident(x)
  } else {
    x
  }
}
