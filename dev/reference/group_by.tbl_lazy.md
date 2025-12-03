# Group by one or more variables

This is a method for the dplyr
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
generic. It is translated to the `GROUP BY` clause of the SQL query when
used with
[`summarise()`](https://dbplyr.tidyverse.org/dev/reference/summarise.tbl_lazy.md)
and to the `PARTITION BY` clause of window functions when used with
[`mutate()`](https://dbplyr.tidyverse.org/dev/reference/mutate.tbl_lazy.md).

## Usage

``` r
# S3 method for class 'tbl_lazy'
group_by(.data, ..., .add = FALSE, .drop = TRUE)
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .add:

  When `FALSE`, the default,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

  This argument was previously called `add`, but that prevented creating
  a new grouping variable called `add`, and conflicts with our naming
  conventions.

- .drop:

  Not supported by this method.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(g = c(1, 1, 1, 2, 2), x = c(4, 3, 6, 9, 2))
db |>
  group_by(g) |>
  summarise(n()) |>
  show_query()
#> <SQL>
#> SELECT `g`, COUNT(*) AS `n()`
#> FROM `dbplyr_tmp_au7izdkFTv`
#> GROUP BY `g`

db |>
  group_by(g) |>
  mutate(x2 = x / sum(x, na.rm = TRUE)) |>
  show_query()
#> <SQL>
#> SELECT `dbplyr_tmp_au7izdkFTv`.*, `x` / SUM(`x`) OVER (PARTITION BY `g`) AS `x2`
#> FROM `dbplyr_tmp_au7izdkFTv`
```
