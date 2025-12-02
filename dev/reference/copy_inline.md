# Use a local data frame in a dbplyr query

This is an alternative to
[`dplyr::copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
that does not need write access and is faster for small data.

## Usage

``` r
copy_inline(con, df, types = NULL)
```

## Arguments

- con:

  A database connection.

- df:

  A local data frame. The data is written directly in the SQL query so
  it should be small.

- types:

  A named character vector of SQL data types to use for the columns. The
  data types are backend specific. For example for Postgres this could
  be `c(id = "bigint", created_at = "timestamp", values = "integer[]")`.
  If `NULL`, the default, the types are determined from `df`.

## Value

A `tbl_lazy`.

## Details

It writes the data directly in the SQL query via the `VALUES` clause.

## See also

[`dplyr::copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
to copy the data into a new database table.

## Examples

``` r
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

copy_inline(con, df)
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.1 [:memory:]
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     2 b    
#> 3     3 c    

copy_inline(con, df) |> dplyr::show_query()
#> <SQL>
#> SELECT CAST(`x` AS INTEGER) AS `x`, CAST(`y` AS TEXT) AS `y`
#> FROM (
#>   SELECT NULL AS `x`, NULL AS `y`
#>   WHERE (0 = 1)
#> 
#>   UNION ALL
#> 
#>   VALUES (1, 'a'), (2, 'b'), (3, 'c')
#> ) AS `values_table`
```
