#' More SQL generics
#'
#' These are new, so not included in dplyr for backward compatibility
#' purposes.
#'
#' @keywords internal
#' @export
sql_escape_logical <- function(con, x) {
  UseMethod("sql_escape_logical")
}
