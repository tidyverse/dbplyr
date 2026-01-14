# Collapse a query into a subquery

[`collapse()`](https://dplyr.tidyverse.org/reference/compute.html)
forces computation of a lazy query by wrapping it in a subquery. This is
not generally needed, but can be useful if you need to work around
database/dbplyr limitations.

## Usage

``` r
# S3 method for class 'tbl_sql'
collapse(x, ...)
```

## Arguments

- x:

  A lazy data frame backed by a database query.

- ...:

  Ignored.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
db |> filter(a <= 2) |> show_query()
#> <SQL>
#> SELECT `dbplyr_tmp_IbwPcLdPaM`.*
#> FROM `dbplyr_tmp_IbwPcLdPaM`
#> WHERE (`a` <= 2.0)
db |> filter(a <= 2) |> collapse() |> show_query()
#> <SQL>
#> SELECT `dbplyr_tmp_IbwPcLdPaM`.*
#> FROM `dbplyr_tmp_IbwPcLdPaM`
#> WHERE (`a` <= 2.0)
```
