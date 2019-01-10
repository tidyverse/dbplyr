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

# DBIConnection methods -----------------------------------------------------------------

#' @export
sql_subquery.DBIConnection <- function(con, from, name = unique_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    build_sql("(", from, ") ", ident(name %||% random_table_name()), con = con)
  }
}

#' @export
sql_escape_string.DBIConnection <- function(con, x) {
  dbQuoteString(con, x)
}

#' @export
sql_escape_string.NULL <- function(con, x) {
  sql_quote(x, "'")
}

#' @export
sql_escape_ident.DBIConnection <- function(con, x) {
  dbQuoteIdentifier(con, x)
}

#' @export
sql_escape_ident.NULL <- function(con, x) {
  sql_quote(x, '"')
}

#' @export
sql_escape_logical.DBIConnection <- function(con, x) {
  y <- as.character(x)
  y[is.na(x)] <- "NULL"

  y
}

#' @export
sql_escape_logical.NULL <- sql_escape_logical.DBIConnection


#' @export
sql_translate_env.NULL <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}

#' @export
sql_translate_env.DBIConnection <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}

