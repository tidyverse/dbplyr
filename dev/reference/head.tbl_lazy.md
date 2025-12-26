# Subset the first rows

This is a method for the [`head()`](https://rdrr.io/r/utils/head.html)
generic. It is usually translated to the `LIMIT` clause of the SQL
query. Because `LIMIT` is not an official part of the SQL specification,
some database use other clauses like `TOP` or `FETCH ROWS`.

Note that databases don't really have a sense of row order, so what
"first" means is subject to interpretation. Most databases will respect
ordering performed with
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html), but
it's not guaranteed. [`tail()`](https://rdrr.io/r/utils/head.html) is
not supported at all because the situation is even murkier for the
"last" rows. Additionally, `LIMIT` clauses can not generally appear in
subqueries, which means that you should use
[`head()`](https://rdrr.io/r/utils/head.html) as late as possible in
your pipelines.

## Usage

``` r
# S3 method for class 'tbl_lazy'
head(x, n = 6L, ...)
```

## Arguments

- x:

  A lazy data frame backed by a database query.

- n:

  Number of rows to return

- ...:

  Not used.

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = 1:100)
db |> head() |> show_query()
#> <SQL>
#> SELECT `dbplyr_tmp_CzqTiZvfqQ`.*
#> FROM `dbplyr_tmp_CzqTiZvfqQ`
#> LIMIT 6

# Pretend we have data in a SQL server database
db2 <- lazy_frame(x = 1:100, con = simulate_mssql())
db2 |> head() |> show_query()
#> <SQL>
#> SELECT TOP 6 [df].*
#> FROM [df]
```
