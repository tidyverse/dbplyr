# Subset distinct/unique rows

This is a method for the dplyr
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
generic. It adds the `DISTINCT` clause to the SQL query.

## Usage

``` r
# S3 method for class 'tbl_lazy'
distinct(.data, ..., .keep_all = FALSE)
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .keep_all:

  If `TRUE`, keep all variables in `.data`. If a combination of `...` is
  not distinct, this keeps the first row of values.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = c(1, 1, 2, 2), y = c(1, 2, 1, 1))
db |> distinct() |> show_query()
#> <SQL>
#> SELECT DISTINCT *
#> FROM `dbplyr_tmp_oeBlLyCUBq`
db |> distinct(x) |> show_query()
#> <SQL>
#> SELECT DISTINCT `x`
#> FROM `dbplyr_tmp_oeBlLyCUBq`
```
