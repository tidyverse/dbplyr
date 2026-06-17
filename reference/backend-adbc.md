# ADBC backend

This backend supports databases accessed via `AdbiConnection` created by
[`adbi::adbi()`](https://adbi.r-dbi.org/reference/dbConnect.html) and
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
dbplyr automatically detects the underlying database type by querying
the ADBC driver's vendor name and uses the appropriate SQL dialect.

The following vendors are recognized:

- PostgreSQL and PostgreSQL-compatible systems (CrateDB, CockroachDB,
  Citus, Neon, ParadeDB, TimescaleDB, Yellowbrick, YugabyteDB, CedarDB):
  [`dialect_postgres()`](https://dbplyr.tidyverse.org/reference/backend-postgres.md)

- MySQL and MySQL-compatible systems (MariaDB, TiDB, Vitess): the
  appropriate dialect
  ([`dialect_mariadb()`](https://dbplyr.tidyverse.org/reference/backend-mysql.md)
  for MariaDB,
  [`dialect_mysql()`](https://dbplyr.tidyverse.org/reference/backend-mysql.md)
  otherwise)

- SQLite:
  [`dialect_sqlite()`](https://dbplyr.tidyverse.org/reference/backend-sqlite.md)

- Microsoft SQL Server:
  [`dialect_mssql()`](https://dbplyr.tidyverse.org/reference/backend-mssql.md)

- Snowflake:
  [`dialect_snowflake()`](https://dbplyr.tidyverse.org/reference/backend-snowflake.md)

- Amazon Redshift:
  [`dialect_redshift()`](https://dbplyr.tidyverse.org/reference/backend-redshift.md)

If your database is not recognized, dbplyr will fall back to a generic
ODBC dialect. In this case, or if dbplyr guesses wrong, you can use
[`with_dialect()`](https://dbplyr.tidyverse.org/reference/with_dialect.md)
to choose a specific dialect.

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/articles/translation-verb.md)
for details of overall translation technology.

## Examples

``` r
# ADBC connections require the adbi package and an ADBC driver.
# Once connected, dbplyr automatically detects the database type:
#
# library(adbi)
# library(dplyr, warn.conflicts = FALSE)
#
# con <- DBI::dbConnect(adbi::adbi("adbcsqlite"), uri = ":memory:")
# tbl(con, "my_table") |> filter(x > 1)
```
