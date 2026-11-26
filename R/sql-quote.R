#' Helper function for quoting sql elements
#'
#' If the closing quote character is present in the string, it will be doubled
#' or escaped with a backslash. `NA`s will be replaced with NULL.
#'
#' @param x Character vector to escape. Strings wrapped in [DBI::SQL()] are
#'   treated as already quoted.
#' @param quote Quote character. Either a length 1 character vector for
#'   symmetric quotes (e.g., `"'"` or `'"'`), or a length 2 character vector
#'   for asymmetric quotes (e.g., `c("[", "]")`).
#' @param backslash If `TRUE`, double backslashes and escape closing quote
#'   characters with a backslash instead of doubling them.
#' @export
#' @return A vector of [sql].
#' @keywords internal
#' @examples
#' sql_quote("abc", "'")
#' sql_quote("I've had a good day", "'")
#' sql_quote("I've had a good day", "'", backslash = TRUE)
#' sql_quote(c("abc", NA), "'")
#'
#' sql_quote(c("abc", NA), c("[", "]"))
sql_quote <- function(x, quote, backslash = FALSE) {
  if (inherits(x, "SQL")) {
    return(sql(unclass(x)))
  }

  check_character(x)
  check_character(quote)
  check_bool(backslash)

  if (length(x) == 0) {
    return(sql())
  }

  if (length(quote) == 1) {
    quote <- rep(quote, 2)
  } else if (length(quote) != 2) {
    cli::cli_abort("{.arg quote} must be length 1 or 2.")
  }

  if (backslash) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    replacement <- paste0("\\", quote[[2]])
  } else {
    replacement <- paste0(quote[[2]], quote[[2]])
  }
  y <- gsub(quote[[2]], replacement, x, fixed = TRUE)
  y <- paste0(quote[[1]], y, quote[[2]])
  y[is.na(x)] <- "NULL"
  names(y) <- names(x)

  sql(y)
}
