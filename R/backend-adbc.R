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
#' dialect. Please file an issue if you'd like support for additional
#' databases.
#'
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology.
#'
#' @seealso [with_dialect()] to use a different dialect if dbplyr guesses
#'   incorrectly, or a more specific translation is available.
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

# Returns the vendor name reported by the ADBC driver.
adbc_vendor_name <- function(con) {
  # ADBC_INFO_VENDOR_NAME has code 0
  info <- as.list(adbcdrivermanager::adbc_connection_get_info(
    con@connection,
    info_codes = 0L
  ))
  info$info_value$string_value
}

# Maps ADBC vendor name to the appropriate dialect.
adbc_vendor_to_dialect <- function(vendor) {
  # Vendor names come from the server itself and the casing may vary, so
  # match case-insensitively.
  vendor <- tolower(vendor)

  if (vendor == "sqlite") {
    dialect_sqlite()
  } else if (
    vendor %in%
      c(
        "postgresql",
        "cedardb",
        "citus",
        "cockroachdb",
        "cratedb",
        "neon",
        "paradedb",
        "timescaledb",
        "yellowbrick",
        "yugabytedb"
      )
  ) {
    dialect_postgres()
  } else if (vendor == "mariadb") {
    dialect_mariadb()
  } else if (vendor %in% c("mysql", "tidb", "vitess")) {
    dialect_mysql()
  } else if (
    vendor %in%
      c(
        "microsoft sql server",
        "mssql",
        "sql server"
      )
  ) {
    dialect_mssql()
  } else if (vendor == "snowflake") {
    dialect_snowflake()
  } else if (vendor %in% c("redshift", "amazon redshift")) {
    dialect_redshift()
  } else {
    # Fall back to generic ODBC dialect
    dialect_odbc()
  }
}
