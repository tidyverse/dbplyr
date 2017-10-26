#' Flag a character vector as SQL identifiers
#'
#' `ident()` takes unquoted strings and flags them as identifiers.
#' `ident_q()` assumes its input has already been quoted, and ensures
#' it does not get quoted again. This is currently used only for
#' for `schema.table`.
#'
#' @param ... A character vector, or name-value pairs
#' @param x An object
#' @export
#' @examples
#' # SQL92 quotes strings with '
#' escape("x")
#'
#' # And identifiers with "
#' ident("x")
#' escape(ident("x"))
#'
#' # You can supply multiple inputs
#' ident(a = "x", b = "y")
#' ident_q(a = "x", b = "y")
ident <- function(...) {
  x <- c(...)
  if (length(x) == 0) return(sql())
  stopifnot(is.character(x))

  structure(x, class = c("ident", "character"))
}
setOldClass(c("ident", "character"))

#' @export
#' @rdname ident
ident_q <- function(...) {
  x <- c(...)
  if (length(x) == 0) return(sql())
  stopifnot(is.character(x))

  structure(x, class = c("ident_q", "ident", "character"))
}
setOldClass(c("ident_q", "ident", "character"))

#' @export
print.ident <- function(x, ...) cat(format(x, ...), sep = "\n")
#' @export
format.ident <- function(x, ...) paste0("<IDENT> ", escape(x))

#' @rdname ident
#' @export
is.ident <- function(x) inherits(x, "ident")

