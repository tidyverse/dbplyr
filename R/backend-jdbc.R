#' JDBC backend
#'
#' @description
#' This backend supports databases accessed via `JDBCConnection` created by
#' `RJDBC::JDBC()` and [DBI::dbConnect()]. dbplyr automatically detects the
#' underlying database type from the JDBC connection class and uses the
#' appropriate SQL dialect.
#'
#' The following databases are recognized via their JDBC connection class:
#'
#' * PostgreSQL (`org.postgresql.*`): [dialect_postgres()]
#' * MySQL (`com.mysql.*`): [dialect_mysql()]
#' * MariaDB (`org.mariadb.*`): [dialect_mariadb()]
#' * SQLite (`org.sqlite.*`): [dialect_sqlite()]
#' * Oracle (`oracle.*`): [dialect_oracle()]
#' * SQL Server (`com.microsoft.sqlserver.*`): [dialect_mssql()]
#' * IBM DB2 (`com.ibm.db2.*`): [dialect_db2()]
#' * SAP HANA (`com.sap.db.*`): [dialect_hana()]
#' * Teradata (`com.teradata.*`): [dialect_teradata()]
#' * Apache Hive (`org.apache.hive.*`): [dialect_hive()]
#' * Apache Spark (`org.apache.spark.*` or `com.simba.spark.*`): [dialect_spark_sql()]
#' * Snowflake (`net.snowflake.*`): [dialect_snowflake()]
#' * Impala (`com.cloudera.impala.*`): [dialect_impala()]
#' * Amazon Redshift (`com.amazon.redshift.*`): [dialect_redshift()]
#'
#' If your database is not recognized, dbplyr will fall back to a generic ODBC
#' dialect. Please file an issue if you'd like support for additional databases.
#'
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology.
#'
#' @seealso [with_dialect()] to use a different dialect if dbplyr guesses
#'   incorrectly, or a more specific translation is available.
#' @name backend-jdbc
#' @examples
#' # JDBC connections require the RJDBC package and a JDBC driver JAR file.
#' # Once connected, dbplyr automatically detects the database type:
#' #
#' # library(RJDBC)
#' # library(dplyr, warn.conflicts = FALSE)
#' #
#' # drv <- JDBC("org.postgresql.Driver", "postgresql.jar")
#' # con <- dbConnect(drv, "jdbc:postgresql://localhost/mydb", "user", "password")
#' # tbl(con, "my_table") |> filter(x > 1)
NULL

#' @export
sql_dialect.JDBCConnection <- function(con) {
  class_name <- jdbc_connection_class(con)
  jdbc_class_to_dialect(class_name)
}

#' @export
dbplyr_edition.JDBCConnection <- function(con) {
  2L
}

# Returns the JDBC connection class name
jdbc_connection_class <- function(con) {
  con@jc$getClass()$getName()
}

# Maps JDBC connection class name to the appropriate dialect
jdbc_class_to_dialect <- function(class_name) {
  if (startsWith(class_name, "org.postgresql.")) {
    dialect_postgres()
  } else if (startsWith(class_name, "org.mariadb.")) {
    # MariaDB (check before MySQL since MariaDB uses different driver)
    dialect_mariadb()
  } else if (startsWith(class_name, "com.mysql.")) {
    dialect_mysql()
  } else if (startsWith(class_name, "org.sqlite.")) {
    dialect_sqlite()
  } else if (startsWith(class_name, "oracle.")) {
    dialect_oracle()
  } else if (startsWith(class_name, "com.microsoft.sqlserver.")) {
    dialect_mssql()
  } else if (startsWith(class_name, "com.ibm.db2.")) {
    dialect_db2()
  } else if (startsWith(class_name, "com.sap.db.")) {
    dialect_hana()
  } else if (startsWith(class_name, "com.teradata.")) {
    dialect_teradata()
  } else if (startsWith(class_name, "org.apache.hive.")) {
    dialect_hive()
  } else if (
    startsWith(class_name, "org.apache.spark.") ||
      startsWith(class_name, "com.simba.spark.")
  ) {
    dialect_spark_sql()
  } else if (startsWith(class_name, "net.snowflake.")) {
    dialect_snowflake()
  } else if (
    startsWith(class_name, "com.cloudera.impala.") ||
      startsWith(class_name, "com.simba.impala.")
  ) {
    dialect_impala()
  } else if (startsWith(class_name, "com.amazon.redshift.")) {
    dialect_redshift()
  } else {
    # Fall back to generic ODBC dialect
    dialect_odbc()
  }
}

register_JDBC_dbExecute <- function(...) {
  # Protect against JDBC defined this
  if (methods::existsMethod(DBI::dbExecute, c("JDBCConnection", "character"))) {
    return()
  }
  methods::setMethod(
    DBI::dbExecute,
    c("JDBCConnection", "character"),
    function(conn, statement, ..., immediate = FALSE) {
      RJDBC::dbSendUpdate(conn, statement, ...)
    },
    where = globalenv()
  )
}
