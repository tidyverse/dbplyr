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
#' @param has_window_clause Does the backend support named window
#'   definitions (the `WINDOW` clause)?
#' @param has_table_alias_with_as Does the backend support using `AS`
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
#' # Create a custom dialect
#' my_dialect <- new_sql_dialect(
#'   "custom",
#'   quote_identifier = function(x) sql_quote(x, "`"),
#'   has_window_clause = TRUE
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
  has_window_clause = FALSE,
  has_table_alias_with_as = TRUE
) {
  check_string(dialect)
  check_function(quote_identifier)
  check_bool(has_window_clause)
  check_bool(has_table_alias_with_as)

  structure(
    list(
      quote_identifier = quote_identifier,
      has = list(
        window_clause = has_window_clause,
        table_alias_with_as = has_table_alias_with_as
      )
    ),
    class = c(paste0("sql_dialect_", dialect), "sql_dialect")
  )
}

#' @export
sql_translation.sql_dialect <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}

#' @export
print.sql_dialect <- function(x, ...) {
  cat("<", class(x)[[1]], ">\n", sep = "")
  invisible(x)
}

is_sql_dialect <- function(x) {
  inherits(x, "sql_dialect")
}

sql_has_table_alias_with_as <- function(con) {
  dialect <- sql_dialect(con)

  if (is_sql_dialect(dialect)) {
    dialect$has$table_alias_with_as
  } else {
    # For backward comptaibility
    TRUE
  }
}

sql_has_window_clause <- function(con) {
  dialect <- sql_dialect(con)

  if (is_sql_dialect(dialect)) {
    dialect$has$window_clause
  } else {
    # For backward comptaibility
    FALSE
  }
}
