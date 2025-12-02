# Arrange rows by column values

This is an method for the dplyr
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
generic. It generates the `ORDER BY` clause of the SQL query. It also
affects the
[`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
of windowed expressions in
[`mutate.tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/mutate.tbl_lazy.md).

Note that `ORDER BY` clauses can not generally appear in subqueries,
which means that you should
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) as
late as possible in your pipelines.

## Usage

``` r
# S3 method for class 'tbl_lazy'
arrange(.data, ..., .by_group = FALSE)
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .by_group:

  If `TRUE`, will sort first by grouping variable. Applies to grouped
  data frames only.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Missing values

Unlike R, most databases sorts `NA` (`NULL`s) at the front. You can can
override this behaviour by explicitly sorting on `is.na(x)`.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
db |> arrange(a) |> show_query()
#> <SQL>
#> SELECT `dbplyr_lKLUUEdLie`.*
#> FROM `dbplyr_lKLUUEdLie`
#> ORDER BY `a`

# Note that NAs are sorted first
db |> arrange(b)
#> # Source:     SQL [?? x 2]
#> # Database:   sqlite 3.51.1 [:memory:]
#> # Ordered by: b
#>       a     b
#>   <dbl> <dbl>
#> 1     2    NA
#> 2     4     1
#> 3     1     2
#> 4     3     5
# override by sorting on is.na() first
db |> arrange(is.na(b), b)
#> # Source:     SQL [?? x 2]
#> # Database:   sqlite 3.51.1 [:memory:]
#> # Ordered by: is.na(b), b
#>       a     b
#>   <dbl> <dbl>
#> 1     4     1
#> 2     1     2
#> 3     3     5
#> 4     2    NA
```
