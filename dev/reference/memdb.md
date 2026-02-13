# A temporary in-memory database

`memdb()` creates a temporary in-memory database that disappears when
the R session ends. It's a convenient way to learn about and experiment
with dbplyr without having to connect to a "real" database.

`memdb_frame()` works like
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html),
but instead of creating a new data frame in R, it creates a table in
`memdb()`. `local_memdb_frame()` is like `memdb_frame()` but the table
will be automatically deleted when the current scope ends. It's useful
for tests. But beware: this function will overwrite an existing table of
the same name.

## Usage

``` r
memdb()

memdb_frame(.name = unique_table_name(), ...)

local_memdb_frame(.name = unique_table_name(), ..., frame = caller_env())
```

## Arguments

- .name:

  Name of table in database: defaults to a random name that's unlikely
  to conflict with an existing table.

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

- frame:

  The created table is bound to this execution frame and will be deleted
  when it ends. For expert use only.

## Examples

``` r
library(dplyr)

# use memdb_frame() to create a new database table
df <- memdb_frame(x = runif(100), y = runif(100))
df |> arrange(x)
#> # A query:    ?? x 2
#> # Database:   sqlite 3.51.2 [:memory:]
#> # Ordered by: x
#>          x     y
#>      <dbl> <dbl>
#>  1 0.00851 0.960
#>  2 0.0162  0.400
#>  3 0.0181  0.295
#>  4 0.0237  0.830
#>  5 0.0275  0.346
#>  6 0.0406  0.213
#>  7 0.0472  0.931
#>  8 0.0612  0.980
#>  9 0.0701  0.324
#> 10 0.0718  0.346
#> # ℹ more rows
df |> arrange(x) |> show_query()
#> <SQL>
#> SELECT *
#> FROM `dbplyr_tmp_INtPiNWcEg`
#> ORDER BY `x`

# Use memdb() + copy_to() to copy an existing data frame
iris_db <- copy_to(memdb(), iris)
iris_db
#> # A query:  ?? x 5
#> # Database: sqlite 3.51.2 [:memory:]
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <chr>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ more rows
```
