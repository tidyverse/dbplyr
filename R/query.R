#' @export
sql_build.query <- function(op, con = NULL, ...) {
  op
}

#' @export
sql_optimise.query <- function(x, con = NULL, ...) {
  # Default to no optimisation
  x
}
