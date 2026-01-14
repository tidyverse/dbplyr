#' Flag a character vector as SQL identifiers
#'
#' @description
#' `ident()` marks strings as database identifiers (e.g. table or column names)
#' quoting them using the identifier rules for your database. It is used
#' primarily in [translate_sql()] to label variables as identifiers; use
#' elsewhere should be regarded with suspicion.
#'
#' `ident()` is for internal use only; if you need to supply an table name that
#' is qualified with schema or catalog use `I()`.
#'
#' @param ... A character vector, or name-value pairs.
#' @param x An object.
#' @keywords internal
#' @export
#' @examples
#' con <- dialect_ansi()
#'
#' # SQL92 quotes strings with '
#' escape("x", con = con)
#'
#' # And identifiers with "
#' escape(ident("x"), con = con)
ident <- function(...) {
  x <- c_character(...)
  structure(x, class = c("ident", "character"))
}

#' @export
print.ident <- function(x, ...) cat(format(x, ...), sep = "\n")
#' @export
format.ident <- function(x, ...) {
  if (length(x) == 0) {
    paste0("<IDENT> [empty]")
  } else {
    paste0("<IDENT> ", x)
  }
}

#' @rdname ident
#' @export
is.ident <- function(x) inherits(x, "ident")

# Needed in in_catalog() and in_schema() for backward compatibility since
# those functions recommended that you use sql() to escape table names.
# Also needed in win_over() since the user might be generating their own
# syntax
as_ident_or_sql <- function(x) {
  if (is.sql(x)) {
    x
  } else if (is.ident(x)) {
    x
  } else if (is_bare_character(x)) {
    ident(x)
  } else {
    cli::cli_abort(
      "Invalid identifier: expecting a character vector.",
      call = call
    )
  }
}
