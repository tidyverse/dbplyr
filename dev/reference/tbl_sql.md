# Create an SQL tbl (abstract)

Generally, you should no longer need to provide a custom
[`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) method. The
default `tbl.DBIConnect` method should work in most cases.

## Usage

``` r
tbl_sql(subclass, src, from, ..., vars = NULL, check_from = deprecated())
```

## Arguments

- subclass:

  name of subclass

- ...:

  needed for agreement with generic. Not otherwise used.

- vars:

  Provide column names as a character vector to avoid retrieving them
  from the database. Mainly useful for better performance when creating
  multiple `tbl` objects.

- check_from:

  **\[deprecated\]**
