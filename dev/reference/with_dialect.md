# Override the SQL dialect for a connection

`with_dialect()` overrides the default dialect assigned to a connection.
This is useful when dbplyr guesses the dialect incorrectly, which is
most likely to occur with ODBC/JDBC/ADBC backends.

## Usage

``` r
with_dialect(con, dialect)
```

## Arguments

- con:

  A database connection (class `DBIConnection`).

- dialect:

  A dialect object created by a `dialect_*()` function (e.g.,
  [`dialect_postgres()`](https://dbplyr.tidyverse.org/dev/reference/backend-postgres.md),
  [`dialect_sqlite()`](https://dbplyr.tidyverse.org/dev/reference/backend-sqlite.md)).

## Value

A connection object that uses the specified dialect for SQL generation.

## Examples

``` r
# Wrap an in-memory SQLite connection to use Postgres dialect
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
pg_con <- with_dialect(con, dialect_postgres())

# SQL generation uses Postgres syntax
lf <- lazy_frame(x = 1, con = pg_con)
lf |> dplyr::mutate(y = sd(x))
#> <SQL>
#> SELECT *, STDDEV_SAMP("x") OVER () AS "y"
#> FROM "df"

DBI::dbDisconnect(con)
```
