#' @export
sql_build.query <- function(x, con = NULL, ...) {
  x
}

#' @export
sql_optimise.query <- function(x, con = NULL, ...) {
  # Default to no optimisation
  x
}

#' @export
sql_optimise.lazy_query <- function(x, con = NULL, ...) {
  # Default to no optimisation
  x
}
