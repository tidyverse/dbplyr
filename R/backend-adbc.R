#' ADBC backend
#'
#' @description
#' This backend supports databases accessed via `AdbiConnection` created by
#' `adbi::adbi()` and [DBI::dbConnect()]. dbplyr automatically detects the
#' underlying database type by querying the ADBC driver's vendor name and
#' uses the appropriate SQL dialect.
#'
#' The following vendors are recognized:
#'
#' * PostgreSQL and PostgreSQL-compatible systems (CrateDB, CockroachDB,
#'   Citus, Neon, ParadeDB, TimescaleDB, Yellowbrick, YugabyteDB, CedarDB):
#'   [dialect_postgres()]
#' * MySQL and MySQL-compatible systems (MariaDB, TiDB, Vitess): the
#'   appropriate dialect ([dialect_mariadb()] for MariaDB, [dialect_mysql()]
#'   otherwise)
#' * SQLite: [dialect_sqlite()]
#' * Microsoft SQL Server: [dialect_mssql()]
#' * Snowflake: [dialect_snowflake()]
#' * Amazon Redshift: [dialect_redshift()]
#'
#' If your database is not recognized, dbplyr will fall back to a generic ODBC
#' dialect. In this case, or if dbplyr guesses wrong, you can use
#' [with_dialect()] to choose a specific dialect.
#'
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology.
#'
#' @name backend-adbc
#' @examples
#' # ADBC connections require the adbi package and an ADBC driver.
#' # Once connected, dbplyr automatically detects the database type:
#' #
#' # library(adbi)
#' # library(dplyr, warn.conflicts = FALSE)
#' #
#' # con <- DBI::dbConnect(adbi::adbi("adbcsqlite"), uri = ":memory:")
#' # tbl(con, "my_table") |> filter(x > 1)
NULL

#' @export
sql_dialect.AdbiConnection <- function(con) {
  vendor <- adbc_vendor_name(con)
  adbc_vendor_to_dialect(vendor)
}

#' @export
dbplyr_edition.AdbiConnection <- function(con) {
  2L
}

adbc_vendor_name <- function(con) {
  info <- as.list(adbcdrivermanager::adbc_connection_get_info(
    con@connection,
    info_codes = 0L # ADBC_INFO_VENDOR_NAME
  ))
  info$info_value$string_value
}

adbc_vendor_to_dialect <- function(vendor) {
  postgres <- dialect_postgres()
  mysql <- dialect_mysql()
  mssql <- dialect_mssql()
  redshift <- dialect_redshift()

  dialects <- list(
    sqlite = dialect_sqlite(),
    postgresql = postgres,
    cedardb = postgres,
    citus = postgres,
    cockroachdb = postgres,
    cratedb = postgres,
    neon = postgres,
    paradedb = postgres,
    timescaledb = postgres,
    yellowbrick = postgres,
    yugabytedb = postgres,
    mariadb = dialect_mariadb(),
    mysql = mysql,
    tidb = mysql,
    vitess = mysql,
    "microsoft sql server" = mssql,
    mssql = mssql,
    "sql server" = mssql,
    snowflake = dialect_snowflake(),
    redshift = redshift,
    "amazon redshift" = redshift
  )

  dialects[[tolower(vendor)]] %||% dialect_odbc()
}
