#' SQL escaping/quoting generics
#'
#' These generics translate individual values into SQL. The core
#' generics are [DBI::dbQuoteIdentifier()] and[DBI::dbQuoteString]
#' for quoting identifiers and strings, but dbplyr needs additional
#' tools for inserting logical, date, date-time, and raw values into
#' queries.
#'
#' @keywords internal
#' @family generic
#' @name db-quote
#' @aliases NULL
#' @examples
#' con <- simulate_dbi()
#' sql_escape_logical(con, c(TRUE, FALSE, NA))
#' sql_escape_date(con, Sys.Date())
#' sql_escape_date(con, Sys.time())
#' sql_escape_raw(con, charToRaw("hi"))
NULL

#' @rdname db-quote
#' @export
sql_escape_logical <- function(con, x, ...) {
  UseMethod("sql_escape_logical")
}
#' @export
sql_escape_logical.DBIConnection <- function(con, x, ...) {
  y <- as.character(x)
  y[is.na(x)] <- "NULL"
  y
}

#' @export
#' @rdname db-quote
sql_escape_date <- function(con, x, ...) {
  UseMethod("sql_escape_date")
}
#' @export
sql_escape_date.DBIConnection <- function(con, x, ...) {
  sql_escape_string(con, as.character(x))
}

#' @export
#' @rdname db-quote
sql_escape_datetime <- function(con, x, ...) {
  UseMethod("sql_escape_datetime")
}
#' @export
sql_escape_datetime.DBIConnection <- function(con, x, ...) {
  x <- strftime(x, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  sql_escape_string(con, x)
}

#' @export
#' @rdname db-quote
sql_escape_raw <- function(con, x, ...) {
  UseMethod("sql_escape_raw")
}
#' @export
sql_escape_raw.DBIConnection <- function(con, x, ...) {
  # SQL-99 standard for BLOB literals
  # https://crate.io/docs/sql-99/en/latest/chapters/05.html#blob-literal-s
  paste0(c("X'", format(x), "'"), collapse = "")
}
