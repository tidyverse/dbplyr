#' Flag a character vector as SQL identifiers
#'
#' This ensures that during SQL generation, the values will be quoted as
#' identifiers, not as strings. `as.ident()` coerces to an identifier,
#' assuming that escaped SQL already represents an identifier.
#'
#' @param ... A character vector, or name-value pairs
#' @param x An object
#' @export
#' @examples
#' # SQL92 quotes strings with '
#' escape("x")
#'
#' # And identifiers with "
#' escape(ident("x"))
ident <- function(...) {
  x <- c(...)
  if (length(x) == 0) return(sql())
  stopifnot(is.character(x))

  structure(x, class = c("ident", "character"))
}
setOldClass(c("ident", "character"))

#' @export
print.ident <- function(x, ...) cat(format(x, ...), sep = "\n")
#' @export
format.ident <- function(x, ...) paste0("<IDENT> ", x)

#' @rdname ident
#' @export
is.ident <- function(x) inherits(x, "ident")

#' @rdname ident
#' @export
as.ident <- function(x) UseMethod("as.ident")

#' @export
as.ident.ident <- function(x) x
#' @export
as.ident.sql <- function(x) x
#' @export
as.ident.character <- function(x) ident(x)
