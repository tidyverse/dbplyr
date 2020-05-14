#' @export
sql_escape_date.PrestoConnection <- function(con, x) {
  paste0('date ', dbQuoteString(con, as.character(x)))
}


#' @export
sql_escape_datetime.PrestoConnection <- function(con, x) {
  x <- strftime(x, "%Y-%m-%d %H:%M:%OS %Z")
  paste0('timestamp ', dbQuoteString(con, x))
}
