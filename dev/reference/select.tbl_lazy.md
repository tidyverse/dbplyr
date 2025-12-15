# Subset, rename, and reorder columns using their names

These are methods for the dplyr
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html),
and
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
generics. They generate the `SELECT` clause of the SQL query.

These functions do not support predicate functions, i.e. you can not use
`where(is.numeric)` to select all numeric variables.

## Usage

``` r
# S3 method for class 'tbl_lazy'
select(.data, ...)

# S3 method for class 'tbl_lazy'
rename(.data, ...)

# S3 method for class 'tbl_lazy'
rename_with(.data, .fn, .cols = everything(), ...)

# S3 method for class 'tbl_lazy'
relocate(.data, ..., .before = NULL, .after = NULL)
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .fn:

  A function used to transform the selected `.cols`. Should return a
  character vector the same length as the input.

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to rename; defaults to all columns.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Destination of columns selected by `...`. Supplying neither will move
  columns to the left-hand side; specifying both is an error.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = 1, y = 2, z = 3)
db |> select(-y) |> show_query()
#> <SQL>
#> SELECT `x`, `z`
#> FROM `dbplyr_tmp_aMbu5uyJ9l`
db |> relocate(z) |> show_query()
#> <SQL>
#> SELECT `z`, `x`, `y`
#> FROM `dbplyr_tmp_aMbu5uyJ9l`
db |> rename(first = x, last = z) |> show_query()
#> <SQL>
#> SELECT `x` AS `first`, `y`, `z` AS `last`
#> FROM `dbplyr_tmp_aMbu5uyJ9l`
```
