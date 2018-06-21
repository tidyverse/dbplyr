#' SQL escaping.
#'
#' These functions are critical when writing functions that translate R
#' functions to sql functions. Typically a conversion function should escape
#' all its inputs and return an sql object.
#'
#' @param ... Character vectors that will be combined into a single SQL
#'   expression.
#' @export
sql <- function(...) {
  x <- c_character(...)
  structure(x, class = c("sql", "character"))
}

# See setOldClass definition in zzz.R

#' @export
c.sql <- function(..., drop_null = FALSE, con = NULL) {
  input <- list(...)
  if (drop_null) input <- compact(input)

  out <- unlist(lapply(input, escape, collapse = NULL, con = con))
  sql(out)
}

#' @export
c.ident <- c.sql

#' @export
unique.sql <- function(x, ...) {
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
    paste0("<SQL> ", x)
  }
}

#' @rdname sql
#' @export
#' @param x Object to coerce
as.sql <- function(x) UseMethod("as.sql")

#' @export
as.sql.ident <- function(x) x
#' @export
as.sql.sql <- function(x) x
#' @export
as.sql.character <- function(x) ident(x)
