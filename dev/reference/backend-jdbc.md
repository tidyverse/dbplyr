# JDBC backend

This backend supports databases accessed via `JDBCConnection` created by
[`RJDBC::JDBC()`](https://rdrr.io/pkg/RJDBC/man/JDBC.html) and
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
dbplyr automatically detects the underlying database type from the JDBC
connection class and uses the appropriate SQL dialect.

The following databases are recognized via their JDBC connection class:

- PostgreSQL (`org.postgresql.*`):
  [`dialect_postgres()`](https://dbplyr.tidyverse.org/dev/reference/backend-postgres.md)

- MySQL (`com.mysql.*`):
  [`dialect_mysql()`](https://dbplyr.tidyverse.org/dev/reference/backend-mysql.md)

- MariaDB (`org.mariadb.*`):
  [`dialect_mariadb()`](https://dbplyr.tidyverse.org/dev/reference/backend-mysql.md)

- SQLite (`org.sqlite.*`):
  [`dialect_sqlite()`](https://dbplyr.tidyverse.org/dev/reference/backend-sqlite.md)

- Oracle (`oracle.*`):
  [`dialect_oracle()`](https://dbplyr.tidyverse.org/dev/reference/backend-oracle.md)

- SQL Server (`com.microsoft.sqlserver.*`):
  [`dialect_mssql()`](https://dbplyr.tidyverse.org/dev/reference/backend-mssql.md)

- SAP HANA (`com.sap.db.*`):
  [`dialect_hana()`](https://dbplyr.tidyverse.org/dev/reference/backend-hana.md)

- Teradata (`com.teradata.*`):
  [`dialect_teradata()`](https://dbplyr.tidyverse.org/dev/reference/backend-teradata.md)

- Apache Hive (`org.apache.hive.*`):
  [`dialect_hive()`](https://dbplyr.tidyverse.org/dev/reference/backend-hive.md)

- Apache Spark (`org.apache.spark.*` or `com.simba.spark.*`):
  [`dialect_spark_sql()`](https://dbplyr.tidyverse.org/dev/reference/backend-spark-sql.md)

- Snowflake (`net.snowflake.*`):
  [`dialect_snowflake()`](https://dbplyr.tidyverse.org/dev/reference/backend-snowflake.md)

- Impala (`com.cloudera.impala.*`):
  [`dialect_impala()`](https://dbplyr.tidyverse.org/dev/reference/backend-impala.md)

- Amazon Redshift (`com.amazon.redshift.*`):
  [`dialect_redshift()`](https://dbplyr.tidyverse.org/dev/reference/backend-redshift.md)

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
