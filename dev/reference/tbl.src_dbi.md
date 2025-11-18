# Use dplyr verbs with a remote database table

All data manipulation on SQL tbls are lazy: they will not actually run
the query or retrieve the data unless you ask for it: they all return a
new `tbl_dbi` object. Use
[`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html)
to run the query and save the results in a temporary table in the
database, or use
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to retrieve the results to R. You can see the query with
[`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html).

## Usage

``` r
# S3 method for class 'src_dbi'
tbl(src, from, ...)
```

## Arguments

- src:

  A `DBIConnection` object produced by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- from:

  Either a table identifier or a literal
  [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) string.

  Use a string to identify a table in the current schema/catalog. We
  recommend using [`I()`](https://rdrr.io/r/base/AsIs.html) to identify
  a table outside the default catalog or schema, e.g.
  `I("schema.table")` or `I("catalog.schema.table")`. You can also use
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)/[`in_catalog()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)
  or [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html).

- ...:

  Passed on to
  [`tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/tbl_sql.md)

## Details

For best performance, the database should have an index on the variables
that you are grouping by. Use
[`dplyr::explain()`](https://dplyr.tidyverse.org/reference/explain.html)
to check that the database is using the indexes that you expect.

There is one verb that is not lazy:
[`dplyr::do()`](https://dplyr.tidyverse.org/reference/do.html) is eager
because it must pull the data into R.

## Examples

``` r
library(dplyr)

# Connect to a temporary in-memory SQLite database
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Add some data
copy_to(con, mtcars)
DBI::dbListTables(con)
#> [1] "mtcars"       "sqlite_stat1" "sqlite_stat4"

# To retrieve a single table from a source, use `tbl()`
con %>% tbl("mtcars")
#> # Source:   table<`mtcars`> [?? x 11]
#> # Database: sqlite 3.51.0 [:memory:]
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
con %>% tbl(I("temp.mtcars")) %>% head(1)
#> # Source:   SQL [?? x 11]
#> # Database: sqlite 3.51.0 [:memory:]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    21     6   160   110   3.9  2.62  16.5     0     1     4     4

# You can also use pass raw SQL if you want a more sophisticated query
con %>% tbl(sql("SELECT * FROM mtcars WHERE cyl = 8"))
#> # Source:   SQL [?? x 11]
#> # Database: sqlite 3.51.0 [:memory:]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  2  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  3  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#>  4  17.3     8  276.   180  3.07  3.73  17.6     0     0     3     3
#>  5  15.2     8  276.   180  3.07  3.78  18       0     0     3     3
#>  6  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4
#>  7  10.4     8  460    215  3     5.42  17.8     0     0     3     4
#>  8  14.7     8  440    230  3.23  5.34  17.4     0     0     3     4
#>  9  15.5     8  318    150  2.76  3.52  16.9     0     0     3     2
#> 10  15.2     8  304    150  3.15  3.44  17.3     0     0     3     2
#> 11  13.3     8  350    245  3.73  3.84  15.4     0     0     3     4
#> 12  19.2     8  400    175  3.08  3.84  17.0     0     0     3     2
#> 13  15.8     8  351    264  4.22  3.17  14.5     0     1     5     4
#> 14  15       8  301    335  3.54  3.57  14.6     0     1     5     8

# If you just want a temporary in-memory database, use src_memdb()
src2 <- src_memdb()

# To show off the full features of dplyr's database integration,
# we'll use the Lahman database. lahman_sqlite() takes care of
# creating the database.

if (requireNamespace("Lahman", quietly = TRUE)) {
batting <- copy_to(con, Lahman::Batting)
batting

# Basic data manipulation verbs work in the same way as with a tibble
batting %>% filter(yearID > 2005, G > 130)
batting %>% select(playerID:lgID)
batting %>% arrange(playerID, desc(yearID))
batting %>% summarise(G = mean(G), n = n())

# There are a few exceptions. For example, databases give integer results
# when dividing one integer by another. Multiply by 1 to fix the problem
batting %>%
  select(playerID:lgID, AB, R, G) %>%
  mutate(
   R_per_game1 = R / G,
   R_per_game2 = R * 1.0 / G
 )

# All operations are lazy: they don't do anything until you request the
# data, either by `print()`ing it (which shows the first ten rows),
# or by `collect()`ing the results locally.
system.time(recent <- filter(batting, yearID > 2010))
system.time(collect(recent))

# You can see the query that dplyr creates with show_query()
batting %>%
  filter(G > 0) %>%
  group_by(playerID) %>%
  summarise(n = n()) %>%
  show_query()
}
#> <SQL>
#> SELECT `playerID`, COUNT(*) AS `n`
#> FROM (
#>   SELECT `Lahman::Batting`.*
#>   FROM `Lahman::Batting`
#>   WHERE (`G` > 0.0)
#> ) AS `q01`
#> GROUP BY `playerID`
```
