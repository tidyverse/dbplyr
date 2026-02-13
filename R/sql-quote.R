#' Helper function for quoting sql elements
#'
#' If the quote character is present in the string, it will be doubled.
#' `NA`s will be replaced with NULL.
#'
#' @param x Character vector to escape.
#' @param quote Quote character. Either a length 1 character vector for
#'   symmetric quotes (e.g., `"'"` or `'"'`), or a length 2 character vector
#'   for asymmetric quotes (e.g., `c("[", "]")`).
#' @export
#' @return A vector of <sql>.
#' @keywords internal
#' @examples
#' sql_quote("abc", "'")
#' sql_quote("I've had a good day", "'")
#' sql_quote(c("abc", NA), "'")
#'
#' sql_quote(c("abc", NA), c("[", "]"))
sql_quote <- function(x, quote) {
  check_character(x)
  check_character(quote)

  if (length(x) == 0) {
    return(sql())
  }

  if (length(quote) == 1) {
    y <- gsub(quote, paste0(quote, quote), x, fixed = TRUE)
    y <- paste0(quote, y, quote)
  } else if (length(quote) == 2) {
    y <- gsub(quote[[2]], paste0(quote[[2]], quote[[2]]), x, fixed = TRUE)
    y <- paste0(quote[[1]], y, quote[[2]])
  } else {
    cli::cli_abort("{.arg quote} must be length 1 or 2.")
  }
  y[is.na(x)] <- "NULL"
  names(y) <- names(x)

  sql(y)
}
