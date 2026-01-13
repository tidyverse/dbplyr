#' Simulate database connections
#'
#' @description
#' These functions generate S3 objects that have been designed to simulate
#' the action of a database connection, without actually having the database
#' available. Obviously, this simulation can only be incomplete, but most
#' importantly it allows us to simulate SQL generation for any database without
#' actually connecting to it.
#'
#' Simulated SQL quotes identifiers with `"x"` (double quotes) by default,
#' `` `x` `` (backticks) for MySQL/MariaDB/SQLite, and `[x]` (square brackets)
#' for SQL Server. Strings are quoted with `'x'`.
#'
#' @keywords internal
#' @export
simulate_dbi <- function(class = character(), ...) {
  structure(
    list(),
    ...,
    class = c(class, "TestConnection", "DBIConnection")
  )
}

dialect_ansi <- function() {
  structure(
    list(
      quote_identifier = function(x) sql_quote(x, '"'),
      has = list(
        window_clause = FALSE,
        table_alias_with_as = TRUE
      )
    ),
    class = "sql_dialect"
  )
}

#' @export
dbplyr_edition.TestConnection <- function(con) 2L

#' @export
sql_escape_ident.TestConnection <- function(con, x) {
  if (inherits(con, "Microsoft SQL Server")) {
    sql_quote(x, c("[", "]"))
  } else if (
    inherits(con, "MySQLConnection") ||
      inherits(con, "MariaDBConnection") ||
      inherits(con, "SQLiteConnection")
  ) {
    sql_quote(x, "`")
  } else {
    sql_quote(x, '"')
  }
}

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
