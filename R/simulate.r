simulate_test <- function() {
  structure(list(), class = c("DBITestConnection", "DBIConnection"))
}

db_query_fields.DBITestConnection <- function(con, sql, ...) {
  c("field1")
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
simulate_mssql <- function() {
  structure(
    list(),
    class = c("Microsoft SQL Server", "OdbcConnection", "DBIConnection")
  )

}

#' @export
#' @rdname tbl_lazy
simulate_hive <- function() {
  structure(
    list(),
    class = c("Hive", "OdbcConnection", "DBIConnection")
  )
}

#' @export
#' @rdname tbl_lazy
simulate_impala <- function() {
  structure(
    list(),
    class = c("Impala", "OdbcConnection", "DBIConnection")
  )
}


