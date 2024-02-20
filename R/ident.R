#' Flag a character vector as SQL identifiers
#'
#' @description
#' `ident()` takes unquoted strings and flags them as database identifiers
#' (e.g. table or column names). `ident_q()` assumes its input has already
#' been quoted, and ensures it does not get quoted again.
#'
#' These are generally for internal use only; if you need to supply an
#' table name that is already quoted, use `I()`.
#'
#' @param ... A character vector, or name-value pairs
#' @keywords internal
#' @param x An object
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

