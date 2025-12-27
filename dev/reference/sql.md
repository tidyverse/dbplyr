# Literal SQL escaping

Use `sql()` to declare that a string is literal SQL and should be used
as is, without quoting.

## Usage

``` r
sql(...)

is.sql(x)
```

## Arguments

- ...:

  Character vectors that will be combined into a single SQL vector.

- x:

  Object to check if it is an sql object.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
# sql() just adds a class
sql("x + 1")
#> <SQL> x + 1
is.sql(sql("x + 1"))
#> [1] TRUE

# You can use it when you need to insert some literal SQL in a query
db <- memdb_frame(x = 1:3)
db |> mutate(y = sql("CAST(x as VARCHAR)"))
#> # A query:  ?? x 2
#> # Database: sqlite 3.51.1 [:memory:]
#>       x y    
#>   <int> <chr>
#> 1     1 1    
#> 2     2 2    
#> 3     3 3    
```
