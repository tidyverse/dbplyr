# Replace NAs with specified values

This is a method for the
[`tidyr::replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html)
generic.

## Usage

``` r
# S3 method for class 'tbl_lazy'
replace_na(data, replace = list(), ...)
```

## Arguments

- data:

  A pair of lazy data frame backed by database queries.

- replace:

  A named list of values, with one value for each column that has NA
  values to be replaced.

- ...:

  Unused; included for compatibility with generic.

## Value

Another `tbl_lazy`. Use
[`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collapse.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
df <- memdb_frame(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% tidyr::replace_na(list(x = 0, y = "unknown"))
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.0 [:memory:]
#>       x y      
#>   <dbl> <chr>  
#> 1     1 a      
#> 2     2 unknown
#> 3     0 b      
```
