# Compute results of a query

These are methods for the dplyr generics
[`dplyr::collapse()`](https://dplyr.tidyverse.org/reference/compute.html),
[`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html),
and
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).
[`collapse()`](https://dplyr.tidyverse.org/reference/compute.html)
creates a subquery,
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) stores
the results in a remote table, and
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
executes the query and downloads the data into R.

## Usage

``` r
# S3 method for class 'tbl_sql'
collapse(x, ...)

# S3 method for class 'tbl_sql'
compute(
  x,
  name = NULL,
  temporary = TRUE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  ...,
  cte = FALSE
)

# S3 method for class 'tbl_sql'
collect(x, ..., n = Inf, warn_incomplete = TRUE, cte = FALSE)
```

## Arguments

- x:

  A lazy data frame backed by a database query.

- ...:

  other parameters passed to methods.

- name:

  Table name in remote database.

- temporary:

  Should the table be temporary (`TRUE`, the default) or persistent
  (`FALSE`)?

- unique_indexes:

  a list of character vectors. Each element of the list will create a
  new unique index over the specified column(s). Duplicate rows will
  result in failure.

- indexes:

  a list of character vectors. Each element of the list will create a
  new index.

- analyze:

  if `TRUE` (the default), will automatically ANALYZE the new table so
  that the query optimiser has useful information.

- cte:

  **\[experimental\]** Use common table expressions in the generated
  SQL?

- n:

  Number of rows to fetch. Defaults to `Inf`, meaning all rows.

- warn_incomplete:

  Warn if `n` is less than the number of result rows?

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
db %>% filter(a <= 2) %>% collect()
#> # A tibble: 2 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     1     2
#> 2     2    NA
```
