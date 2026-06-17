# Miscellaneous database generics

These are used when creating a new dbplyr backend and should generally
not be called directly.

- `db_connection_describe()` provides a short string describing the
  database connection, helping users tell which database a table comes
  from. It should be a single line, and ideally less than 60 characters
  wide.

- `dbplyr_edition()` declares which version of the dbplyr API you want.

- `db_col_types()` introspects an existing table and returns a named
  character vector mapping column names to their database-native SQL
  types. The result is suitable for use as the `field.types` argument of
  [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html),
  allowing dbplyr to preserve column types when copying data with
  `rows_*()`.

  Returns `NULL` if the backend does not implement introspection.

## Usage

``` r
db_connection_describe(con, ...)

sql_join_suffix(con, suffix, ...)

db_sql_render(con, sql, ..., cte = FALSE, sql_options = NULL)

db_col_types(con, table, call)

dbplyr_edition(con)
```

## Arguments

- con:

  A database connection.

- table:

  A table identifier, or `NULL`. Use a string to refer to tables in the
  current schema/catalog or [`I()`](https://rdrr.io/r/base/AsIs.html) to
  refer to tables in other schemas/catalogs.

- call:

  The execution environment of a currently running function, used to
  report errors.

## See also

Other generic:
[`db-sql`](https://dbplyr.tidyverse.org/reference/db-sql.md),
[`db_copy_to()`](https://dbplyr.tidyverse.org/reference/db-io.md),
[`escape()`](https://dbplyr.tidyverse.org/reference/escape.md)
