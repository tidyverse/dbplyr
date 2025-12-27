# Summarise each group to one row

This is a method for the dplyr
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
generic. It generates the `SELECT` clause of the SQL query, and
generally needs to be combined with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Usage

``` r
# S3 method for class 'tbl_lazy'
summarise(.data, ..., .by = NULL, .groups = NULL)
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

- .groups:

  **\[experimental\]** Grouping structure of the result.

  - "drop_last": dropping the last level of grouping. This was the only
    supported option before version 1.0.0.

  - "drop": All levels of grouping are dropped.

  - "keep": Same grouping structure as `.data`.

  When `.groups` is not specified, it defaults to "drop_last".

  In addition, a message informs you of that choice, unless the result
  is ungrouped, the option "dplyr.summarise.inform" is set to `FALSE`,
  or when
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  is called from a function in a package.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(g = c(1, 1, 1, 2, 2), x = c(4, 3, 6, 9, 2))
db |>
  summarise(n()) |>
  show_query()
#> <SQL>
#> SELECT COUNT(*) AS `n()`
#> FROM `dbplyr_tmp_yOK9MyP4tf`

db |>
  group_by(g) |>
  summarise(n()) |>
  show_query()
#> <SQL>
#> SELECT `g`, COUNT(*) AS `n()`
#> FROM `dbplyr_tmp_yOK9MyP4tf`
#> GROUP BY `g`
```
