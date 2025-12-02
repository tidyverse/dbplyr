# Expand SQL tables to include all possible combinations of values

This is a method for the
[tidyr::expand](https://tidyr.tidyverse.org/reference/expand.html)
generics. It doesn't sort the result explicitly, so the order might be
different to what `expand()` returns for data frames.

## Usage

``` r
# S3 method for class 'tbl_lazy'
expand(data, ..., .name_repair = "check_unique")
```

## Arguments

- data:

  A lazy data frame backed by a database query.

- ...:

  Specification of columns to expand. See
  [tidyr::expand](https://tidyr.tidyverse.org/reference/expand.html) for
  more details.

- .name_repair:

  Treatment of problematic column names:

  - `"minimal"`: No name repair or checks, beyond basic existence,

  - `"unique"`: Make sure names are unique and not empty,

  - `"check_unique"`: (default value), no name repair, but check they
    are `unique`,

  - `"universal"`: Make the names `unique` and syntactic

  - `"unique_quiet"`: Same as `"unique"`, but "quiet"

  - `"universal_quiet"`: Same as `"universal"`, but "quiet"

  - a function: apply custom name repair (e.g.,
    `.name_repair = make.names` for names in the style of base R).

  - A purrr-style anonymous function, see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)

  This argument is passed on as `repair` to
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html).
  See there for more details on these terms and the strategies used to
  enforce them.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
fruits <- memdb_frame(
  type   = c("apple", "orange", "apple", "orange", "orange", "orange"),
  year   = c(2010, 2010, 2012, 2010, 2010, 2012),
  size = c("XS", "S",  "M", "S", "S", "M"),
  weights = rnorm(6)
)

# All possible combinations ---------------------------------------
fruits |> tidyr::expand(type)
#> # Source:   SQL [?? x 1]
#> # Database: sqlite 3.51.1 [:memory:]
#>   type  
#>   <chr> 
#> 1 apple 
#> 2 orange
fruits |> tidyr::expand(type, size)
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.1 [:memory:]
#>   type   size 
#>   <chr>  <chr>
#> 1 apple  XS   
#> 2 apple  S    
#> 3 apple  M    
#> 4 orange XS   
#> 5 orange S    
#> 6 orange M    

# Only combinations that already appear in the data ---------------
fruits |> tidyr::expand(nesting(type, size))
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.1 [:memory:]
#>   type   size 
#>   <chr>  <chr>
#> 1 apple  XS   
#> 2 orange S    
#> 3 apple  M    
#> 4 orange M    
```
