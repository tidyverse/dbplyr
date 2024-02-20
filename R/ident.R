#' Flag a character vector as SQL identifiers
#'
#' @description
#' `ident()` takes strings and turns them as database identifiers (e.g. table
#' or column names) quoting them using the identifer rules for your database.
#' `ident_q()` does the same, but assumes the names have already been
#' quoted, preventing them from being quoted again.
#'
#' These are generally for internal use only; if you need to supply an
#' table name that is qualified with schema or catalog, or has already been
#' quoted for some other reason, use `I()`.
#'
#' @param ... A character vector, or name-value pairs.
#' @param x An object.
#' @keywords internal
#' @export
#' @examples
#' # SQL92 quotes strings with '
#' escape_ansi("x")
#'
#' # And identifiers with "
#' ident("x")
#' escape_ansi(ident("x"))
#'
#' # You can supply multiple inputs
#' ident(a = "x", b = "y")
#' ident_q(a = "x", b = "y")
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

