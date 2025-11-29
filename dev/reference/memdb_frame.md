# Create a database table in temporary in-memory database.

`memdb_frame()` works like
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html),
but instead of creating a new data frame in R, it creates a table in
`src_memdb()`.

## Usage

``` r
memdb_frame(..., .name = unique_table_name())

tbl_memdb(df, name = deparse(substitute(df)))

src_memdb()
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  A set of name-value pairs. These arguments are processed with
  [`rlang::quos()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  and support unquote via
  [`rlang::!!`](https://rlang.r-lib.org/reference/injection-operator.html)
  and unquote-splice via
  [`rlang::!!!`](https://rlang.r-lib.org/reference/splice-operator.html).
  Use `:=` to create columns that start with a dot.

  Arguments are evaluated sequentially. You can refer to previously
  created elements directly or using the
  [rlang::.data](https://rlang.r-lib.org/reference/dot-data.html)
  pronoun. To refer explicitly to objects in the calling environment,
  use
  [`rlang::!!`](https://rlang.r-lib.org/reference/injection-operator.html)
  or [rlang::.env](https://rlang.r-lib.org/reference/dot-data.html),
  e.g. `!!.data` or `.env$.data` for the special case of an object named
  `.data`.

- df:

  Data frame to copy

- name, .name:

  Name of table in database: defaults to a random name that's unlikely
  to conflict with an existing table.

## Examples

``` r
library(dplyr)
df <- memdb_frame(x = runif(100), y = runif(100))
df |> arrange(x)
#> # Source:     SQL [?? x 2]
#> # Database:   sqlite 3.51.0 [:memory:]
#> # Ordered by: x
#>         x      y
#>     <dbl>  <dbl>
#>  1 0.0110 0.270 
#>  2 0.0372 0.996 
#>  3 0.0478 0.614 
#>  4 0.0587 0.171 
#>  5 0.0626 0.0612
#>  6 0.0743 0.344 
#>  7 0.0805 0.400 
#>  8 0.0939 0.886 
#>  9 0.100  0.502 
#> 10 0.150  0.918 
#> # ℹ more rows
df |> arrange(x) |> show_query()
#> <SQL>
#> SELECT `dbplyr_aByGT89T45`.*
#> FROM `dbplyr_aByGT89T45`
#> ORDER BY `x`

mtcars_db <- tbl_memdb(mtcars)
mtcars_db |> group_by(cyl) |> summarise(n = n()) |> show_query()
#> <SQL>
#> SELECT `cyl`, COUNT(*) AS `n`
#> FROM `mtcars`
#> GROUP BY `cyl`
```
