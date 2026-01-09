#' SQL dialects
#'
#' @description
#' The dialect system allows multiple database connection classes to share
#' SQL generation code. A dialect object encapsulates the SQL syntax rules
#' for a particular database, independent of the connection mechanism.
#'
#' * `sql_dialect()` returns the dialect for a connection. For connections
#'   that haven't implemented a dialect method, returns the connection itself
#'   for backward compatibility.
#'
#' * `new_sql_dialect()` creates a new dialect object. This is primarily
#'   intended for dbplyr backend authors.
#'
#' @param con A database connection or dialect object.
#' @param dialect A string giving the dialect name (e.g., "postgres", "mysql").
#' @param quote_identifier A function that quotes identifiers. Should accept
#'   a character vector and return a [sql] vector.
#' @param supports_window_clause Does the backend support named window
#'   definitions (the `WINDOW` clause)?
#' @param supports_table_alias_with_as Does the backend support using `AS`
#'   when aliasing a table in a subquery?
#'
#' @return
#' * `sql_dialect()` returns a dialect object (class `sql_dialect`) or
#'   the connection itself for backward compatibility.
#' * `new_sql_dialect()` returns a dialect object with class
#'   `c("sql_dialect_{name}", "sql_dialect")`.
#'
#' @export
#' @examples
#' # Get dialect for a connection
#' sql_dialect(dialect_postgres())
#'
#' # Create a custom dialect
#' my_dialect <- new_sql_dialect(
#'   "custom",
#'   quote_identifier = function(x) sql_quote(x, "`"),
#'   supports_window_clause = TRUE
#' )
#' class(my_dialect)
sql_dialect <- function(con) {
  UseMethod("sql_dialect")
}

#' @export
sql_dialect.sql_dialect <- function(con) {
  con
}

#' @export
sql_dialect.DBIConnection <- function(con) {
  # For backward compatibility: connections without a dialect method
  # pass through unchanged
  con
}

#' @export
#' @rdname sql_dialect
new_sql_dialect <- function(
  dialect,
  quote_identifier,
  supports_window_clause = FALSE,
  supports_table_alias_with_as = TRUE
) {
  check_string(dialect)
  check_function(quote_identifier)
  check_bool(supports_window_clause)
  check_bool(supports_table_alias_with_as)

  structure(
    list(
      quote_identifier = quote_identifier,
      supports_window_clause = supports_window_clause,
      supports_table_alias_with_as = supports_table_alias_with_as
    ),
    class = c(paste0("sql_dialect_", dialect), "sql_dialect")
  )
}

dialect_ansi <- function() {
  new_sql_dialect(
    "ansi",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}

#' @export
sql_translation.sql_dialect_ansi <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}

#' @export
print.sql_dialect <- function(x, ...) {
  cat("<sql_dialect>\n")
  cat("Class:", paste(class(x), collapse = ", "), "\n")
  invisible(x)
}

#' @export
dbplyr_edition.sql_dialect <- function(con) {
  2L
}

sql_has_table_alias_with_as <- function(con) {
  dialect <- sql_dialect(con)

  if (inherits(dialect, "sql_dialect")) {
    dialect$supports_table_alias_with_as
  } else if (inherits(dialect, "DBIConnection")) {
    TRUE
  } else {
    cli::cli_abort(
      "{.arg con} must be a DBIConnection or sql_dialect.",
      .internal = TRUE
    )
  }
}

sql_has_window_clause <- function(con) {
  dialect <- sql_dialect(con)

  if (inherits(dialect, "sql_dialect")) {
    dialect$supports_window_clause
  } else if (inherits(dialect, "DBIConnection")) {
    FALSE
  } else {
    cli::cli_abort(
      "{.arg con} must be a DBIConnection or sql_dialect.",
      .internal = TRUE
    )
  }
}
