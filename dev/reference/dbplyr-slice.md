# Subset rows using their positions

These are methods for the dplyr generics
[`dplyr::slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_max()`](https://dplyr.tidyverse.org/reference/slice.html),
and
[`dplyr::slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html).
They are translated to SQL using
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and window functions (`ROWNUMBER`, `MIN_RANK`, or `CUME_DIST` depending
on arguments).
[`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
[`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html), and
[`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html) are
not supported since database tables have no intrinsic order.

If data is grouped, the operation will be performed on each group so
that (e.g.) `slice_min(db, x, n = 3)` will select the three rows with
the smallest value of `x` in each group.

## Usage

``` r
# S3 method for class 'tbl_lazy'
slice_min(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = TRUE
)

# S3 method for class 'tbl_lazy'
slice_max(
  .data,
  order_by,
  ...,
  n,
  by = NULL,
  prop,
  with_ties = TRUE,
  na_rm = TRUE
)

# S3 method for class 'tbl_lazy'
slice_sample(.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE)
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- order_by:

  Variable or function of variables to order by.

- ...:

  Not used.

- n, prop:

  Provide either `n`, the number of rows, or `prop`, the proportion of
  rows to select. If neither are supplied, `n = 1` will be used.

  If `n` is greater than the number of rows in the group (or `prop` \>
  1), the result will be silently truncated to the group size. If the
  proportion of a group size is not an integer, it is rounded down.

- by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- with_ties:

  Should ties be kept together? The default, `TRUE`, may return more
  rows than you request. Use FALSE to ignore ties, and return the first
  n rows.

- na_rm:

  Should missing values in `order_by` be removed from the result? If
  `FALSE`, `NA` values are sorted to the end (like in
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)), so
  they will only be included if there are insufficient non-missing
  values to reach `n`/`prop`.

- weight_by, replace:

  Not supported for database backends.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = 1:3, y = c(1, 1, 2))
db |> slice_min(x) |> show_query()
#> <SQL>
#> SELECT `x`, `y`
#> FROM (
#>   SELECT `dbplyr_tmp_pYJGCySHwm`.*, RANK() OVER (ORDER BY `x`) AS `col01`
#>   FROM `dbplyr_tmp_pYJGCySHwm`
#> ) AS `q01`
#> WHERE (`col01` <= 1)
db |> slice_max(x) |> show_query()
#> <SQL>
#> SELECT `x`, `y`
#> FROM (
#>   SELECT `dbplyr_tmp_pYJGCySHwm`.*, RANK() OVER (ORDER BY `x` DESC) AS `col01`
#>   FROM `dbplyr_tmp_pYJGCySHwm`
#> ) AS `q01`
#> WHERE (`col01` <= 1)
db |> slice_sample() |> show_query()
#> <SQL>
#> SELECT `x`, `y`
#> FROM (
#>   SELECT
#>     `dbplyr_tmp_pYJGCySHwm`.*,
#>     ROW_NUMBER() OVER (ORDER BY (0.5 + RANDOM() / 18446744073709551616.0)) AS `col01`
#>   FROM `dbplyr_tmp_pYJGCySHwm`
#> ) AS `q01`
#> WHERE (`col01` <= 1)

db |> group_by(y) |> slice_min(x) |> show_query()
#> <SQL>
#> SELECT `x`, `y`
#> FROM (
#>   SELECT
#>     `dbplyr_tmp_pYJGCySHwm`.*,
#>     RANK() OVER (PARTITION BY `y` ORDER BY `x`) AS `col01`
#>   FROM `dbplyr_tmp_pYJGCySHwm`
#> ) AS `q01`
#> WHERE (`col01` <= 1)

# By default, ties are includes so you may get more rows
# than you expect
db |> slice_min(y, n = 1)
#> # A query:  ?? x 2
#> # Database: sqlite 3.51.1 [:memory:]
#>       x     y
#>   <int> <dbl>
#> 1     1     1
#> 2     2     1
db |> slice_min(y, n = 1, with_ties = FALSE)
#> # A query:  ?? x 2
#> # Database: sqlite 3.51.1 [:memory:]
#>       x     y
#>   <int> <dbl>
#> 1     1     1

# Non-integer group sizes are rounded down
db |> slice_min(x, prop = 0.5)
#> # A query:  ?? x 2
#> # Database: sqlite 3.51.1 [:memory:]
#>       x     y
#>   <int> <dbl>
#> 1     1     1
```
