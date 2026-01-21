# JDBC backend

This backend supports databases accessed via `JDBCConnection` created by
[`RJDBC::JDBC()`](https://rdrr.io/pkg/RJDBC/man/JDBC.html) and
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
dbplyr automatically detects the underlying database type from the JDBC
connection class and uses the appropriate SQL dialect.

The following databases are recognized via their JDBC connection class:

- PostgreSQL (`org.postgresql.*`)

- MySQL (`com.mysql.*`)

- MariaDB (`org.mariadb.*`)

- SQLite (`org.sqlite.*`)

- Oracle (`oracle.*`)

- SQL Server (`com.microsoft.sqlserver.*`)

- SAP HANA (`com.sap.db.*`)

- Teradata (`com.teradata.*`)

- Apache Hive (`org.apache.hive.*`)

- Apache Spark (`org.apache.spark.*` or `com.simba.spark.*`)

- Snowflake (`net.snowflake.*`)

- Impala (`com.cloudera.impala.*`)

- Amazon Redshift (`com.amazon.redshift.*`)

If your database is not recognized, dbplyr will fall back to a generic
ODBC dialect. Please file an issue if you'd like support for additional
databases.

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## See also

[`with_dialect()`](https://dbplyr.tidyverse.org/dev/reference/with_dialect.md)
to use a different dialect if dbplyr guesses incorrectly, or a more
specific translation is available.

## Examples

``` r
# JDBC connections require the RJDBC package and a JDBC driver JAR file.
# Once connected, dbplyr automatically detects the database type:
#
# library(RJDBC)
# library(dplyr, warn.conflicts = FALSE)
#
# drv <- JDBC("org.postgresql.Driver", "postgresql.jar")
# con <- dbConnect(drv, "jdbc:postgresql://localhost/mydb", "user", "password")
# tbl(con, "my_table") |> filter(x > 1)
```
