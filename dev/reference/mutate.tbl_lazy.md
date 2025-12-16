# Create, modify, and delete columns

These are methods for the dplyr
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
and
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
generics. They are translated to computed expressions in the `SELECT`
clause of the SQL query.

## Usage

``` r
# S3 method for class 'tbl_lazy'
mutate(
  .data,
  ...,
  .by = NULL,
  .order = NULL,
  .frame = NULL,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
)
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

- .order:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  A selection of columns to control ordering for window functions within
  this
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  call. Use [`c()`](https://rdrr.io/r/base/c.html) to order by multiple
  columns, e.g. `.order = c(x, y)`. Each column can be wrapped in
  [`dplyr::desc()`](https://dplyr.tidyverse.org/reference/desc.html) to
  specify descending order. Equivalent to calling
  [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  before and clearing it after the
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- .frame:

  A length-2 numeric vector specifying the bounds for window function
  frames. The first element is the lower bound (use `-Inf` for
  "unbounded preceding") and the second is the upper bound (use `Inf`
  for "unbounded following", `0` for "current row"). Equivalent to
  calling
  [`window_frame()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  before and clearing it after the
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- .keep:

  Control which columns from `.data` are retained in the output.
  Grouping columns and columns created by `...` are always kept.

  - `"all"` retains all columns from `.data`. This is the default.

  - `"used"` retains only the columns used in `...` to create new
    columns. This is useful for checking your work, as it displays
    inputs and outputs side-by-side.

  - `"unused"` retains only the columns *not* used in `...` to create
    new columns. This is useful if you generate new columns, but no
    longer need the columns used to generate them.

  - `"none"` doesn't retain any extra columns from `.data`. Only the
    grouping variables and columns created by `...` are kept.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, control where new columns should appear (the default is to
  add to the right hand side). See
  [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
  for more details.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = 1:5, y = 5:1)
db |>
  mutate(a = (x + y) / 2, b = sqrt(x^2L + y^2L)) |>
  show_query()
#> <SQL>
#> SELECT
#>   `dbplyr_tmp_I6Ojtgl00s`.*,
#>   (`x` + `y`) / 2.0 AS `a`,
#>   SQRT((POWER(`x`, 2)) + POWER(`y`, 2)) AS `b`
#> FROM `dbplyr_tmp_I6Ojtgl00s`

# dbplyr automatically creates subqueries as needed
db |>
  mutate(x1 = x + 1, x2 = x1 * 2) |>
  show_query()
#> <SQL>
#> SELECT `q01`.*, `x1` * 2.0 AS `x2`
#> FROM (
#>   SELECT `dbplyr_tmp_I6Ojtgl00s`.*, `x` + 1.0 AS `x1`
#>   FROM `dbplyr_tmp_I6Ojtgl00s`
#> ) AS `q01`

# `.order` and `.frame` control window functions
db <- memdb_frame(g = c(1, 1, 2, 2, 2), x = c(5, 3, 1, 4, 2))
db |>
  mutate(rolling_sum = sum(x), .by = g, .order = x, .frame = c(-2, 2)) |>
  show_query()
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.
#> <SQL>
#> SELECT
#>   `dbplyr_tmp_IlBpEtJTVT`.*,
#>   SUM(`x`) OVER (PARTITION BY `g` ORDER BY `x` ROWS BETWEEN 2 PRECEDING AND 2 FOLLOWING) AS `rolling_sum`
#> FROM `dbplyr_tmp_IlBpEtJTVT`
```
