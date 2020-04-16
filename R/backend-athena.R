#' @export
sql_escape_date.AthenaConnection <- function(con, x) {
  paste0('date ', dbQuoteString(con, as.character(x)))
}


#' @export
sql_escape_datetime.AthenaConnection <- function(con, x) {
  x <- strftime(x, "%Y-%m-%d %H:%M:%OS %Z")
  paste0('timestamp ', dbQuoteString(con, x))
}
