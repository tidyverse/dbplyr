# Pivot data from long to wide

`pivot_wider()` "widens" data, increasing the number of columns and
decreasing the number of rows. The inverse transformation is
`pivot_longer()`. Learn more in
[`vignette("pivot", "tidyr")`](https://tidyr.tidyverse.org/articles/pivot.html).

`pivot_wider()` on database tables comes with some caveats, please make
sure to read below for details.

## Usage

``` r
# S3 method for class 'tbl_lazy'
pivot_wider(
  data,
  ...,
  id_cols = NULL,
  id_expand = FALSE,
  names_from = name,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_vary = "fastest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = ~max(.x, na.rm = TRUE),
  unused_fn = NULL
)

dbplyr_pivot_wider_spec(
  data,
  spec,
  ...,
  names_repair = "check_unique",
  id_cols = NULL,
  id_expand = FALSE,
  values_fill = NULL,
  values_fn = ~max(.x, na.rm = TRUE),
  unused_fn = NULL,
  error_call = current_env()
)
```

## Arguments

- data:

  A lazy data frame backed by a database query.

- ...:

  Unused; included for compatibility with generic.

- id_cols:

  A set of columns that uniquely identifies each observation.

- id_expand:

  Unused; included for compatibility with the generic.

- names_from, values_from:

  A pair of arguments describing which column (or columns) to get the
  name of the output column (`names_from`), and which column (or
  columns) to get the cell values from (`values_from`).

  If `values_from` contains multiple values, the value will be added to
  the front of the output column.

- names_prefix:

  String added to the start of every variable name.

- names_sep:

  If `names_from` or `values_from` contains multiple variables, this
  will be used to join their values together into a single string to use
  as a column name.

- names_glue:

  Instead of `names_sep` and `names_prefix`, you can supply a glue
  specification that uses the `names_from` columns (and special
  `.value`) to create custom column names.

- names_sort:

  Should the column names be sorted? If `FALSE`, the default, column
  names are ordered by first appearance.

- names_vary:

  When `names_from` identifies a column (or columns) with multiple
  unique values, and multiple `values_from` columns are provided, in
  what order should the resulting column names be combined?

  - `"fastest"` varies `names_from` values fastest, resulting in a
    column naming scheme of the form:
    `value1_name1, value1_name2, value2_name1, value2_name2`. This is
    the default.

  - `"slowest"` varies `names_from` values slowest, resulting in a
    column naming scheme of the form:
    `value1_name1, value2_name1, value1_name2, value2_name2`.

- names_expand:

  Should the values in the `names_from` columns be expanded by
  [`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html)
  before pivoting? This results in more columns, the output will contain
  column names corresponding to a complete expansion of all possible
  values in `names_from`. Additionally, the column names will be sorted,
  identical to what `names_sort` would produce.

- names_repair:

  What happens if the output has invalid column names?

- values_fill:

  Optionally, a (scalar) value that specifies what each `value` should
  be filled in with when missing. Be careful when using this in
  combination with the default `values_fn`.

- values_fn:

  A function, the default is
  [`max()`](https://rdrr.io/r/base/Extremes.html), applied to the
  `value` in each cell in the output. In contrast to local data frames
  it must not be `NULL`.

- unused_fn:

  Optionally, a function applied to summarize the values from the unused
  columns (i.e. columns not identified by `id_cols`, `names_from`, or
  `values_from`).

  The default drops all unused columns from the result.

  This can be a named list if you want to apply different aggregations
  to different unused columns.

  `id_cols` must be supplied for `unused_fn` to be useful, since
  otherwise all unspecified columns will be considered `id_cols`.

  This is similar to grouping by the `id_cols` then summarizing the
  unused columns using `unused_fn`.

- spec:

  A specification data frame. This is useful for more complex pivots
  because it gives you greater control on how metadata stored in the
  columns become column names in the result.

  Must be a data frame containing character `.name` and `.value`
  columns. Additional columns in `spec` should be named to match columns
  in the long format of the dataset and contain values corresponding to
  columns pivoted from the wide format. The special `.seq` variable is
  used to disambiguate rows internally; it is automatically removed
  after pivoting.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Caveats

### `pivot_wider()` is eager

Note that `pivot_wider()` cannot be lazy because we need to look at the
data to figure out what the new column names will be. If you have a
long-running query you have two options:

- Temporarily store the result of the query via
  [`compute()`](https://dplyr.tidyverse.org/reference/compute.html).

- Create a spec before and use `dbplyr_pivot_wider_spec()` - dbplyr's
  version of
  [`tidyr::pivot_wider_spec()`](https://tidyr.tidyverse.org/reference/pivot_wider_spec.html).

### You must supply `values_fn`

The big difference to `pivot_wider()` for local data frames is that
`values_fn` must not be `NULL`. By default it is
[`max()`](https://rdrr.io/r/base/Extremes.html) which yields the same
results as for local data frames if three conditions are true:

1.  The combination of `id_cols` and `value` uniquely identify an
    observation.

2.  The column has a comparable type (e.g. numeric, date-time, or (for
    most databases) string).

3.  `values_fill` is `NULL`.

If either the second or third condition is not met, you must supply a
custom `values_fn`. Unfortunately there is no generally available
alternative and you'll need to look for something database specific,
like `FIRST()` or `ANY_VALUE()`.

## How does it work?

The translation to SQL code basically works as follows:

1.  Get unique keys in `names_from` column.

2.  For each key value generate an expression of the form:

        values_fn(
          CASE WHEN (`names from column` == `key value`) THEN (`value column`) END
        ) AS `output column`

3.  Group data by id columns.

4.  Summarise the grouped data with the expressions from step 2.

## Examples

``` r
memdb_frame(
  id = 1,
  key = c("x", "y"),
  value = 1:2
) |>
  tidyr::pivot_wider(
    id_cols = id,
    names_from = key,
    values_from = value
  )
#> # A query:  ?? x 3
#> # Database: sqlite 3.51.2 [:memory:]
#>      id     x     y
#>   <dbl> <int> <int>
#> 1     1     1     2
```
