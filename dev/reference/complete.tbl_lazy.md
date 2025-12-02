# Complete a SQL table with missing combinations of data

Turns implicit missing values into explicit missing values. This is a
method for the
[`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
generic.

## Usage

``` r
# S3 method for class 'tbl_lazy'
complete(data, ..., fill = list())
```

## Arguments

- data:

  A lazy data frame backed by a database query.

- ...:

  Specification of columns to expand. See
  [tidyr::expand](https://tidyr.tidyverse.org/reference/expand.html) for
  more details.

- fill:

  A named list that for each variable supplies a single value to use
  instead of NA for missing combinations.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
df <- memdb_frame(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)

df |> tidyr::complete(group, nesting(item_id, item_name))
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.1 [:memory:]
#>   group item_id item_name value1 value2
#>   <dbl>   <dbl> <chr>      <int>  <int>
#> 1     1       1 a              1      4
#> 2     1       2 b              3      6
#> 3     2       1 a             NA     NA
#> 4     2       2 b              2      5

# You can also choose to fill in missing values
df |> tidyr::complete(group, nesting(item_id, item_name), fill = list(value1 = 0))
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.1 [:memory:]
#>   group item_id item_name value1 value2
#>   <dbl>   <dbl> <chr>      <dbl>  <int>
#> 1     1       1 a              1      4
#> 2     1       2 b              3      6
#> 3     2       1 a              0     NA
#> 4     2       2 b              2      5
```
