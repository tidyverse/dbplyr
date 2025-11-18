# Database versions of the nycflights13 data

These functions cache the data from the `nycflights13` database in a
local database, for use in examples and vignettes. Indexes are created
to making joining tables on natural keys efficient.

## Usage

``` r
nycflights13_sqlite(path = NULL)

nycflights13_postgres(dbname = "nycflights13", ...)

has_nycflights13(type = c("sqlite", "postgres"), ...)

copy_nycflights13(con, ...)
```

## Arguments

- path:

  location of SQLite database file

- dbname, ...:

  Arguments passed on to
  [`dplyr::src_postgres()`](https://dplyr.tidyverse.org/reference/src_dbi.html)
