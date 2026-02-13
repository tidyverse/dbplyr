# Create an SQL tbl (abstract)

This function creates a lazy tbl object from a table in a database. It's
primarily intended for backend authors who need to create custom
subclasses; most users should use
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
instead.

## Usage

``` r
tbl_sql(subclass, src, from, ..., vars = NULL, check_from = deprecated())
```

## Arguments

- subclass:

  name of subclass

- src:

  A `DBIConnection` object produced by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- from:

  Either a table identifier or a literal
  [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) string.

  Use a string to identify a table in the current schema/catalog or
  [`I()`](https://rdrr.io/r/base/AsIs.html) for a table elsewhere, e.g.
  `I("schema.table")` or `I("catalog.schema.table")`. For backward
  compatibility, you can also use
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)/[`in_catalog()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  or [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html).

- ...:

  needed for agreement with generic. Not otherwise used.

- vars:

  Optionally, provide a character vector of column names. If not
  supplied, will be retrieved from the database by running a simple
  query. This argument is mainly useful for better performance when
  creating many `tbl`s with known variables.

- check_from:

  **\[deprecated\]**
