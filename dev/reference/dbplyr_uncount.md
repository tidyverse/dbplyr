# "Uncount" a database table

This is a method for the tidyr `uncount()` generic. It uses a temporary
table, so your database user needs permissions to create one.

## Usage

``` r
dbplyr_uncount(data, weights, .remove = TRUE, .id = NULL)
```

## Arguments

- data:

  A lazy data frame backed by a database query.

- weights:

  A vector of weights. Evaluated in the context of `data`; supports
  quasiquotation.

- .remove:

  If `TRUE`, and `weights` is the name of a column in `data`, then this
  column is removed.

- .id:

  Supply a string to create a new variable which gives a unique
  identifier for each created row.

## Examples

``` r
df <- memdb_frame(x = c("a", "b"), n = c(1, 2))
dbplyr_uncount(df, n)
#> # Source:   SQL [?? x 1]
#> # Database: sqlite 3.51.1 [:memory:]
#>   x    
#>   <chr>
#> 1 a    
#> 2 b    
#> 3 b    
dbplyr_uncount(df, n, .id = "id")
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.1 [:memory:]
#>   x        id
#>   <chr> <int>
#> 1 a         1
#> 2 b         1
#> 3 b         2

# You can also use constants
dbplyr_uncount(df, 2)
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.1 [:memory:]
#>   x         n
#>   <chr> <dbl>
#> 1 a         1
#> 2 a         1
#> 3 b         2
#> 4 b         2

# Or expressions
dbplyr_uncount(df, 2 / n)
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.1 [:memory:]
#>   x         n
#>   <chr> <dbl>
#> 1 a         1
#> 2 a         1
#> 3 b         2
```
