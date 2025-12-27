#' Flag a character vector as SQL identifiers
#'
#' @description
#' `ident()` takes strings and turns them as database identifiers (e.g. table
#' or column names) quoting them using the identifier rules for your database.
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
#' con <- simulate_dbi()
#'
#' # SQL92 quotes strings with '
#' escape("x", con = con)
#'
#' # And identifiers with "
#' ident("x")
#' escape(ident("x"), con = con)
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
