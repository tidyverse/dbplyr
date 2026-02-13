# Create a lazy query backed by a database

Use [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) to create
a SQL query backed by a database. Manipulating this object with dplyr
verbs then builds up a SQL query that will only be executed when you
explicitly ask for it, either by printing the object, calling
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to bring the data back to R or calling
[`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html)
to create a new table in the database. You can see the query without
executing it with
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md).

Learn more in
[`vignette("dbplyr")`](https://dbplyr.tidyverse.org/dev/articles/dbplyr.md).

## Usage

``` r
# S3 method for class 'src_dbi'
tbl(src, from, vars = NULL, ...)
```

## Arguments

- src:

  A `DBIConnection` object produced by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- from:

  Either a table identifier or a literal
  [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) string.

  Use a string to identify a table in the current schema/catalog or
  [`I()`](https://rdrr.io/r/base/AsIs.html) for a table elsewhere, e.g.
  `I("schema.table")` or `I("catalog.schema.table")`. For backward
  compatibility, you can also use
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)/[`in_catalog()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  or [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html).

- vars:

  Optionally, provide a character vector of column names. If not
  supplied, will be retrieved from the database by running a simple
  query. This argument is mainly useful for better performance when
  creating many `tbl`s with known variables.

- ...:

  Passed on to
  [`tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/tbl_sql.md)

## Examples

``` r
library(dplyr)

# Connect to a temporary in-memory SQLite database and add some data
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mtcars)

# To retrieve a single table from a source, use `tbl()`
mtcars_db <- con |> tbl("mtcars")
mtcars_db
#> # A query:  ?? x 11
#> # Database: sqlite 3.51.2 [:memory:]
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

# Use `I()` for qualified table names
con |> tbl(I("temp.mtcars")) |> head(1)
#> # A query:  ?? x 11
#> # Database: sqlite 3.51.2 [:memory:]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    21     6   160   110   3.9  2.62  16.5     0     1     4     4

# You can also pass raw SQL if you want a more sophisticated query
con |> tbl(sql("SELECT * FROM mtcars WHERE cyl = 8")) |> head(1)
#> # A query:  ?? x 11
#> # Database: sqlite 3.51.2 [:memory:]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2

# But in most cases, you'll rely on dbplyr to construct the SQL:
mtcars_db |>
  filter(vs == 1) |>
  summarise(mpg = mean(mpg, na.rm = TRUE), .by = cyl) |>
  show_query()
#> <SQL>
#> SELECT `cyl`, AVG(`mpg`) AS `mpg`
#> FROM `mtcars`
#> WHERE (`vs` = 1.0)
#> GROUP BY `cyl`
```
