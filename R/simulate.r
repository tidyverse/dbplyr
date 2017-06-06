simulate_test <- function() {
  structure(list(), class = c("DBITestConnection", "DBIConnection"))
}

db_query_fields.DBITestConnection <- function(con, sql, ...) {
  c("field1")
}

sql_escape_ident.DBITestConnection <- function(con, x) {
  sql_quote(x, "`")
}

sql_escape_string.DBITestConnection <- function(con, x) {
  sql_quote(x, "'")
}

#' @export
sql_subquery.DBITestConnection <- function(con, from, name = unique_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    build_sql("(", from, ") ", ident(name %||% random_table_name()), con = con)
  }
}

# DBI connections --------------------------------------------------------------

#' @export
#' @rdname tbl_lazy
simulate_dbi <- function() {
  structure(
    list(),
    class = "DBIConnection"
  )
}

#' @export
#' @rdname tbl_lazy
simulate_sqlite <- function() {
  structure(
    list(),
    class = c("SQLiteConnection", "DBIConnection")
  )
}

#' @export
#' @rdname tbl_lazy
simulate_postgres <- function() {
  structure(
    list(),
    class = c("PostgreSQLConnection", "DBIConnection")
  )
}

#' @export
#' @rdname tbl_lazy
simulate_mysql <- function() {
  structure(
    list(),
    class = c("MySQLConnection", "DBIConnection")
  )
}

#' @export
#' @rdname tbl_lazy
simulate_odbc <- function(type = NULL) {
  structure(
    list(),
    class = c(type, "DBITestConnection", "DBIConnection")
  )
}

#' @export
#' @rdname tbl_lazy
simulate_impala <- function() simulate_odbc("Impala")

#' @export
#' @rdname tbl_lazy
simulate_mssql <- function() simulate_odbc("Microsoft SQL Server")

#' @export
#' @rdname tbl_lazy
simulate_oracle <- function() simulate_odbc("Oracle")

#' @export
#' @rdname tbl_lazy
simulate_hive <- function() simulate_odbc("Hive")

#' @export
#' @rdname tbl_lazy
simulate_odbc_postgresql <- function() simulate_odbc("PostgreSQL")

