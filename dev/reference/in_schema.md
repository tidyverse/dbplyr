# Refer to a table in another schema/catalog

`in_schema()` and `in_catalog()` can be used to refer to tables outside
of the current catalog/schema. However, we now recommend using
[`I()`](https://rdrr.io/r/base/AsIs.html) as it's typically less typing.

## Usage

``` r
in_schema(schema, table)

in_catalog(catalog, schema, table)
```

## Arguments

- catalog, schema, table:

  Names of catalog, schema, and table. These will be automatically
  quoted; use
  [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) to pass a
  raw name that won't get quoted.

## Examples

``` r
# Previously:
in_schema("my_schema", "my_table")
#> <SCHEMA> `my_schema`.`my_table`
in_catalog("my_catalog", "my_schema", "my_table")
#> <CATALOG> `my_catalog`.`my_schema`.`my_table`
in_schema(sql("my_schema"), sql("my_table"))
#> <SCHEMA> my_schema.my_table

# Now
I("my_schema.my_table")
#> [1] "my_schema.my_table"
I("my_catalog.my_schema.my_table")
#> [1] "my_catalog.my_schema.my_table"
I("my_schema.my_table")
#> [1] "my_schema.my_table"

# Example using schemas with SQLite
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Add auxiliary schema
tmp <- tempfile()
DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS aux"))
#> [1] 0

library(dplyr, warn.conflicts = FALSE)
copy_to(con, iris, "df", temporary = FALSE)
copy_to(con, mtcars, I("aux.df"), temporary = FALSE)

con |> tbl("df")
#> # Source:   table<`df`> [?? x 5]
#> # Database: sqlite 3.51.1 [:memory:]
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
con |> tbl(I("aux.df"))
#> # Source:   table<aux.df> [?? x 11]
#> # Database: sqlite 3.51.1 [:memory:]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ more rows
```
