#' Backend: DatabaseConnector
#'
#' @description
#' DatabaseConnector is a DBI-compliant database backend that supports
#' multiple database systems. It uses JDBC drivers to establish connections,
#' and the connection object includes a \code{dbms} slot that specifies the
#' database type. This backend enables dplyr's SQL translations to work across
#' all database supported platforms by reusing translations
#' from other backends.
#'
#' Use `simulate_databaseconnector(dbms = "sql server")` with `lazy_frame()`
#' to see simulated SQL without connecting to live access database.
#'
#' Supported DatabaseConnector JDBC backends are:
#' \itemize{
#'   \item \code{"oracle"}
#'   \item \code{"postgresql"}
#'   \item \code{"redshift"}
#'   \item \code{"sql server"}
#'   \item \code{"bigquery"}
#'   \item \code{"spark"}
#'   \item \code{"snowflake"}
#'   \item \code{"synapse"}
#'   \item \code{"iris"}
#' }
#'
#' @name backend-databaseconnector
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_databaseconnector(dbms = "oracle"))
#' lf %>% transmute(x = paste0(c, " times"))
#' lf %>% setdiff(lf)
NULL

#' @export
#' @rdname backend-databaseconnector
simulate_databaseconnector <- function(dbms = "sql server") {
  simulate_dbi("DatabaseConnectorJdbcConnection", dbms = dbms)
}

#' @export
#' @importFrom dbplyr dbplyr_edition
#'
#' @param con Database connection
dbplyr_edition.DatabaseConnectorConnection <- function(con) {
  2L
}

#' @export
sql_query_select.DatabaseConnectorJdbcConnection <- function(con,
                                                             select,
                                                             from,
                                                             where = NULL,
                                                             group_by = NULL,
                                                             having = NULL,
                                                             window = NULL,
                                                             order_by = NULL,
                                                             limit = NULL,
                                                             distinct = FALSE,
                                                             ...,
                                                             subquery = FALSE,
                                                             lvl = 0) {

  if (isS4(con)) {
    dbms <- con@dbms
  } else {
    # for testing with simulated connections
    dbms <- attr(con, "dbms")
  }

  switch(dbms,
         "sql server" = `sql_query_select.Microsoft SQL Server`(con,
                                                                select,
                                                                from,
                                                                where,
                                                                group_by,
                                                                having,
                                                                window,
                                                                order_by,
                                                                limit,
                                                                distinct,
                                                                ...,
                                                                subquery,
                                                                lvl),
         "oracle" = sql_query_select.Oracle(con,
                                            select,
                                            from,
                                            where,
                                            group_by,
                                            having,
                                            window,
                                            order_by,
                                            limit,
                                            distinct,
                                            ...,
                                            subquery,
                                            lvl),
         NextMethod()
  )
}

#' @export
sql_translation.DatabaseConnectorJdbcConnection <- function(con) {

  if (isS4(con)) {
    dbms <- con@dbms
  } else {
    dbms <- attr(con, "dbms")
  }

  switch(dbms,
         "oracle" = sql_translation.Oracle(con),
         "postgresql" = sql_translation.PqConnection(con),
         "redshift" = sql_translation.RedshiftConnection(con),
         "sql server" = `sql_translation.Microsoft SQL Server`(con),
         "bigquery" = sql_translation.BigQueryConnection(con),
         "spark" = `sql_translation.Spark SQL`(con),
         "snowflake" = sql_translation.Snowflake(con),
         "synapse" = `sql_translation.Microsoft SQL Server`(con),
         "iris" = sql_translation.PqConnection(con),
         rlang::abort("Sql dialect is not supported!"))
}

#' @export
sql_escape_logical.DatabaseConnectorJdbcConnection <- function(con, x) {

  if (isS4(con)) {
    dbms <- con@dbms
  } else {
    dbms <- attr(con, "dbms")
  }

  if (dbms == "sql server") {
    dplyr::if_else(x, "1", "0", missing = "NULL")
  } else {
    NextMethod()
  }
}

