#' @export
#' @rdname sql_build
sql_optimise <- function(x, con = NULL, ...) {
  UseMethod("sql_optimise")
}

#' @export
sql_optimise.sql <- function(x, con = NULL, ...) {
  # Can't optimise raw SQL
  x
}

#' @export
sql_optimise.ident <- function(x, con = NULL, ...) {
  x
}
