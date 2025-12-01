# Collect results into a local data frame

[`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
executes the query and retrieves the results into a local tibble. This
brings all the data from the database into R's memory, which is useful
once you've done as much as possible in the database, and now need to
use R functions.

## Usage

``` r
# S3 method for class 'tbl_sql'
collect(x, ..., n = Inf, warn_incomplete = TRUE, cte = FALSE)
```

## Arguments

- x:

  A lazy data frame backed by a database query.

- ...:

  Ignored.

- n:

  Number of rows to fetch. Defaults to `Inf`, meaning all rows.

- warn_incomplete:

  Warn if `n` is less than the number of result rows?

- cte:

  **\[experimental\]** Use common table expressions in the generated
  SQL?

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
db |> filter(a <= 2) |> collect()
#> # A tibble: 2 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     1     2
#> 2     2    NA
```
