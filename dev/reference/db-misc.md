# Miscellaneous database generics

- `db_connection_describe()` provides a short string describing the
  database connection, helping users tell which database a table comes
  from. It should be a single line, and ideally less than 60 characters
  wide.

## Usage

``` r
db_connection_describe(con, ...)

sql_join_suffix(con, suffix, ...)

db_sql_render(con, sql, ..., cte = FALSE, sql_options = NULL)

db_col_types(con, table, call)

dbplyr_edition(con)
```

## Details

- `dbplyr_edition()` declares which version of the dbplyr API you want.
  See below for more details.

- `db_col_types()` returns the column types of a table.

## dbplyr 2.0.0

dbplyr 2.0.0 renamed a number of generics so that they could be cleanly
moved from dplyr to dbplyr. If you have an existing backend, you'll need
to rename the following methods.

- [`dplyr::db_desc()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  -\> `dbplyr::db_connection_describe()` (also note that the argument
  named changed from `x` to `con`).

## See also

Other generic:
[`db-sql`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
[`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md),
[`sql_escape_logical()`](https://dbplyr.tidyverse.org/dev/reference/db-quote.md)
