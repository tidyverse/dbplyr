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

# See setOldClass definition in zzz.R

# c() is also called outside of the dbplyr context so must supply default
# connection - this seems like a design mistake, and probably an indication
# that within dbplyr c() should be replaced with a more specific function
#' @export
c.sql <- function(..., drop_null = FALSE, con = simulate_dbi()) {
  input <- list(...)

  if (drop_null) {
    input <- purrr::compact(input)
  } # nocov

  out <- unlist(lapply(input, escape, collapse = NULL, con = con))
  sql(out)
}

#' @export
c.ident <- c.sql

#' @export
unique.sql <- function(x, ...) {
  sql(NextMethod()) # nocov
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

#' @rdname sql
#' @export
#' @param x Object to coerce
#' @param con Needed when `x` is directly supplied from the user so that
#'   schema specifications can be quoted using the correct identifiers.
as.sql <- function(x, con) UseMethod("as.sql")

#' @export
as.sql.ident <- function(x, con) x
#' @export
as.sql.sql <- function(x, con) x
#' @export
as.sql.character <- function(x, con) ident(x)
