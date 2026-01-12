# Database I/O generics

These generics are responsible for getting data into and out of the
database. They should be used a last resort - only use them when you
can't make a backend work by providing methods for DBI generics, or for
dbplyr's SQL generation generics. They tend to be most needed when a
backend has special handling of temporary tables.

- `db_copy_to()` implements
  [`copy_to.src_sql()`](https://dbplyr.tidyverse.org/dev/reference/copy_to.src_sql.md)
  by calling
  [`db_write_table()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  (which calls
  [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html))
  to transfer the data, then optionally adds indexes (via
  [`sql_table_index()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md))
  and analyses (via
  [`sql_table_analyze()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)).

- `db_compute()` implements
  [`compute.tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/compute.tbl_sql.md)
  by calling
  [`sql_query_save()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  to create the table, then optionally adds indexes (via
  [`sql_table_index()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md))
  and analyses (via
  [`sql_table_analyze()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)).

- `db_collect()` implements
  [`collect.tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
  using
  [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
  and [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html).

- `db_table_temporary()` is used for databases that have special naming
  schemes for temporary tables (e.g. SQL server and SAP HANA require
  temporary tables to start with `#`)

- `db_table_drop_if_exists()` is used to drop a table if it exists. This
  is used when `overwrite = TRUE` in
  [`dplyr::copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
  and
  [`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html).

## Usage

``` r
db_copy_to(
  con,
  table,
  values,
  ...,
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  in_transaction = TRUE
)

db_compute(
  con,
  table,
  sql,
  ...,
  overwrite = FALSE,
  temporary = TRUE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  in_transaction = TRUE
)

db_collect(con, sql, n = -1, warn_incomplete = TRUE, ...)

db_table_temporary(con, table, temporary, ...)

db_table_drop_if_exists(con, table, ...)
```

## See also

Other generic:
[`db-sql`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
[`db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md),
[`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
