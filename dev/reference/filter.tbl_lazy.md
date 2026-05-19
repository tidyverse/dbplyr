# Keep or drop rows that match a condition

These are methods for the dplyr
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and
[`dplyr::filter_out()`](https://dplyr.tidyverse.org/reference/filter.html)
generics. They generate the `WHERE` clause of the SQL query.

[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) is
translated directly to `WHERE`, which already matches dplyr's behaviour
of treating `NA` like `FALSE` (SQL's three-valued logic drops `NULL`
rows from `WHERE`).

[`filter_out()`](https://dplyr.tidyverse.org/reference/filter.html)
requires an additional step, where the combined condition is wrapped in
`is_distinct_from(., TRUE)`, which is then translated using the backend
(e.g. to `IS DISTINCT FROM` on PostgreSQL, `IS NOT` on SQLite). This
ensures that the SQL translation matches dplyr's semantics.

## Usage

``` r
# S3 method for class 'tbl_lazy'
filter(.data, ..., .by = NULL, .preserve = FALSE)

# S3 method for class 'tbl_lazy'
filter_out(.data, ..., .by = NULL, .preserve = FALSE)
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
#> FROM `dbplyr_tmp_Pg2ZRmBV2z`
#> WHERE (`x` < 5.0)
db |> filter_out(x < 5) |> show_query()
#> <SQL>
#> SELECT *
#> FROM `dbplyr_tmp_Pg2ZRmBV2z`
#> WHERE ((`x` < 5.0) IS NOT (1))
db |> filter(is.na(x)) |> show_query()
#> <SQL>
#> SELECT *
#> FROM `dbplyr_tmp_Pg2ZRmBV2z`
#> WHERE ((`x` IS NULL))
```
