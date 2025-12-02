# Pivot data from wide to long

`pivot_longer()` "lengthens" data, increasing the number of rows and
decreasing the number of columns. The inverse transformation is
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

Learn more in
[`vignette("pivot", "tidyr")`](https://tidyr.tidyverse.org/articles/pivot.html).

While most functionality is identical there are some differences to
`pivot_longer()` on local data frames:

- the output is sorted differently/not explicitly,

- the coercion of mixed column types is left to the database,

- `values_ptypes` NOT supported.

Note that `build_longer_spec()` and `pivot_longer_spec()` do not work
with remote tables.

## Usage

``` r
# S3 method for class 'tbl_lazy'
pivot_longer(
  data,
  cols,
  ...,
  cols_vary,
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = NULL,
  names_transform = NULL,
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes,
  values_transform = NULL
)
```

## Arguments

- data:

  A data frame to pivot.

- cols:

  Columns to pivot into longer format.

- ...:

  Additional arguments passed on to methods.

- cols_vary:

  Unsupported; included for compatibility with the generic.

- names_to:

  A string specifying the name of the column to create from the data
  stored in the column names of `data`.

- names_prefix:

  A regular expression used to remove matching text from the start of
  each variable name.

- names_sep, names_pattern:

  If `names_to` contains multiple values, these arguments control how
  the column name is broken up.

- names_ptypes:

  A list of column name-prototype pairs.

- names_transform, values_transform:

  A list of column name-function pairs.

- names_repair:

  What happens if the output has invalid column names?

- values_to:

  A string specifying the name of the column to create from the data
  stored in cell values. If `names_to` is a character containing the
  special `.value` sentinel, this value will be ignored, and the name of
  the value column will be derived from part of the existing column
  names.

- values_drop_na:

  If `TRUE`, will drop rows that contain only `NA`s in the `value_to`
  column.

- values_ptypes:

  Not supported.

## Details

The SQL translation basically works as follows:

1.  split the specification by its key columns i.e. by variables crammed
    into the column names.

2.  for each part in the split specification
    [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
    `data` into the following columns

- id columns i.e. columns that are not pivotted

- key columns

- value columns i.e. columns that are pivotted

1.  combine all the parts with
    [`union_all()`](https://dplyr.tidyverse.org/reference/setops.html)

## Examples

``` r
# See vignette("pivot") for examples and explanation

# Simplest case where column names are character data
memdb_frame(
  id = c("a", "b"),
  x = 1:2,
  y = 3:4
) |>
  tidyr::pivot_longer(-id)
#> # Source:   SQL [?? x 3]
#> # Database: sqlite 3.51.1 [:memory:]
#>   id    name  value
#>   <chr> <chr> <int>
#> 1 a     x         1
#> 2 b     x         2
#> 3 a     y         3
#> 4 b     y         4
```
