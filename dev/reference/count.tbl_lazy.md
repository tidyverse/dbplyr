# Count observations by group

These are methods for the dplyr
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html) and
[`dplyr::tally()`](https://dplyr.tidyverse.org/reference/count.html)
generics. They wrap up
[`group_by.tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/group_by.tbl_lazy.md),
[`summarise.tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/summarise.tbl_lazy.md)
and, optionally,
[`arrange.tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/arrange.tbl_lazy.md).

## Usage

``` r
# S3 method for class 'tbl_lazy'
count(x, ..., wt = NULL, sort = FALSE, name = NULL)

# S3 method for class 'tbl_lazy'
add_count(x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = NULL)

# S3 method for class 'tbl_lazy'
tally(x, wt = NULL, sort = FALSE, name = NULL)
```

## Arguments

- x:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Frequency weights. Can be `NULL` or a variable:

  - If `NULL` (the default), counts the number of rows in each group.

  - If a variable, computes `sum(wt)` for each group.

- sort:

  If `TRUE`, will show the largest groups at the top.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- .drop:

  Not supported for lazy tables.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(g = c(1, 1, 1, 2, 2), x = c(4, 3, 6, 9, 2))
db |> count(g) |> show_query()
#> <SQL>
#> SELECT `g`, COUNT(*) AS `n`
#> FROM `dbplyr_rKPRds3Z4q`
#> GROUP BY `g`
db |> count(g, wt = x) |> show_query()
#> <SQL>
#> SELECT `g`, SUM(`x`) AS `n`
#> FROM `dbplyr_rKPRds3Z4q`
#> GROUP BY `g`
db |> count(g, wt = x, sort = TRUE) |> show_query()
#> <SQL>
#> SELECT `g`, SUM(`x`) AS `n`
#> FROM `dbplyr_rKPRds3Z4q`
#> GROUP BY `g`
#> ORDER BY `n` DESC
```
