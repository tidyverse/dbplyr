#' A set of DBI methods to ease unit testing dplyr with DBI
#' @name src-test
#' @export
#' @param con A database connection.
#' @param x Object to transform
#' @param sql A string containing an sql query.
#' @param ... Other arguments passed on to the individual methods
DBITest <- function() {
  structure(list(), class = "DBITestConnection")
}

#' @export
#' @rdname src-test
db_query_fields.DBITestConnection <- function(con, sql, ...) {
  c("field1")
}

#' @export
#' @rdname src-test
sql_escape_ident.DBITestConnection <- function(con, x) {
  sql_quote(x, "`")
}

#' @export
#' @rdname src-test
sql_translate_env.DBITestConnection <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_scalar),
    aggregate = sql_translator(.parent = base_agg),
    window = sql_translator(.parent = base_win)
  )
}
