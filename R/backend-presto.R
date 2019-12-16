#' @export
sql_escape_Date.PrestoConnection <- function(con, x) {
  paste0('date ', dbQuoteString(con, x))
}


#' @export
sql_escape_POSIXt.PrestoConnection <- function(con, x) {
  x <- strftime(x, "%Y-%m-%d %H:%M:%OS %Z")
  paste0('timestamp ', dbQuoteString(con, x))
}
