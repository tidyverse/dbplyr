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
[`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collapse.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = 1:5, y = 5:1)
db %>%
  mutate(a = (x + y) / 2, b = sqrt(x^2L + y^2L)) %>%
  show_query()
#> <SQL>
#> SELECT
#>   `dbplyr_fLBw8zKN1y`.*,
#>   (`x` + `y`) / 2.0 AS `a`,
#>   SQRT((POWER(`x`, 2)) + POWER(`y`, 2)) AS `b`
#> FROM `dbplyr_fLBw8zKN1y`

# dbplyr automatically creates subqueries as needed
db %>%
  mutate(x1 = x + 1, x2 = x1 * 2) %>%
  show_query()
#> <SQL>
#> SELECT `q01`.*, `x1` * 2.0 AS `x2`
#> FROM (
#>   SELECT `dbplyr_fLBw8zKN1y`.*, `x` + 1.0 AS `x1`
#>   FROM `dbplyr_fLBw8zKN1y`
#> ) AS `q01`
```
