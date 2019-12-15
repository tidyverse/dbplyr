#' @export
sql_optimise.query <- function(x, con = NULL, ...) {
  # Default to no optimisation
  x
}


#' Query generics
#'
#' These generics allow backends to customise the SQL generated for various
#' types of query. The default `DBIConnection` methods generate ANSI 92
#' compliant SQL.
#'
#' @keywords internal
#' @param con A database connection.
#' @name generic-query
NULL
