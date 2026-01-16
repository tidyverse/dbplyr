# Subset rows using column values

This is a method for the dplyr
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
generic. It generates the `WHERE` clause of the SQL query.

## Usage

``` r
# S3 method for class 'tbl_lazy'
filter(.data, ..., .by = NULL, .preserve = FALSE)
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .preserve:

  Not supported by this method.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = c(2, NA, 5, NA, 10), y = 1:5)
db |> filter(x < 5) |> show_query()
#> <SQL>
#> SELECT *
#> FROM `dbplyr_tmp_RTXYj8SWnm`
#> WHERE (`x` < 5.0)
db |> filter(is.na(x)) |> show_query()
#> <SQL>
#> SELECT *
#> FROM `dbplyr_tmp_RTXYj8SWnm`
#> WHERE ((`x` IS NULL))
```
