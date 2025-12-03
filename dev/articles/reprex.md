# Reprexes for dbplyr

If you’re reporting a bug in dbplyr, it is much easier for me to help
you if you can supply a [reprex](https://reprex.tidyverse.org) that I
can run on my computer. Creating reprexes for dbplyr is particularly
challenging because you are probably using a database that you can’t
share with me. Fortunately, in many cases you can still demonstrate the
problem even if I don’t have the complete dataset, or even access to the
database system that you’re using.

This vignette outlines three approaches for creating reprexes that will
work anywhere:

- Use
  [`memdb_frame()`](https://dbplyr.tidyverse.org/dev/reference/memdb_frame.md)/[`tbl_memdb()`](https://dbplyr.tidyverse.org/dev/reference/memdb_frame.md)
  to easily create datasets that live in an in-memory SQLite database.

- Use
  [`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)/[`tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
  to simulate SQL generation of dplyr pipelines.

- Use
  [`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
  to simulate SQL generation of columnar expression.

``` r
library(dplyr)
library(dbplyr)
```

## Using `memdb_frame()`

The first place to start is with SQLite. SQLite is particularly
appealing because it’s completely embedded instead an R package so
doesn’t have any external dependencies. SQLite is designed to be small
and simple, so it can’t demonstrate all problems, but it’s easy to try
out and a great place to start.

You can easily create a SQLite in-memory database table using
[`memdb_frame()`](https://dbplyr.tidyverse.org/dev/reference/memdb_frame.md):

``` r
mf <- memdb_frame(g = c(1, 1, 2, 2, 2), x = 1:5, y = 5:1)
mf
#> # Source:   table<`dbplyr_tmp_SwlKLUUEdL`> [?? x 3]
#> # Database: sqlite 3.51.1 [:memory:]
#>       g     x     y
#>   <dbl> <int> <int>
#> 1     1     1     5
#> 2     1     2     4
#> 3     2     3     3
#> 4     2     4     2
#> 5     2     5     1

mf |> 
  group_by(g) |> 
  summarise_all(mean, na.rm = TRUE)
#> # Source:   SQL [?? x 3]
#> # Database: sqlite 3.51.1 [:memory:]
#>       g     x     y
#>   <dbl> <dbl> <dbl>
#> 1     1   1.5   4.5
#> 2     2   4     2
```

Reprexes are easiest to understand if you create very small custom data,
but if you do want to use an existing data frame you can use
[`tbl_memdb()`](https://dbplyr.tidyverse.org/dev/reference/memdb_frame.md):

``` r
mtcars_db <- tbl_memdb(mtcars)
mtcars_db |> 
  group_by(cyl) |> 
  summarise(n = n()) |> 
  show_query()
#> <SQL>
#> SELECT `cyl`, COUNT(*) AS `n`
#> FROM `mtcars`
#> GROUP BY `cyl`
```

## Translating verbs

Many problems with dbplyr come down to incorrect SQL generation.
Fortunately, it’s possible to generate SQL without a database using
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
and
[`tbl_lazy()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md).
Both take an `con` argument which takes a database “simulator” like
[`simulate_postgres()`](https://dbplyr.tidyverse.org/dev/reference/backend-postgres.md),
[`simulate_sqlite()`](https://dbplyr.tidyverse.org/dev/reference/backend-sqlite.md),
etc.

``` r
x <- c("abc", "def", "ghif")

lazy_frame(x = x, con = simulate_postgres()) |> 
  head(5) |> 
  show_query()
#> <SQL>
#> SELECT `df`.*
#> FROM `df`
#> LIMIT 5

lazy_frame(x = x, con = simulate_mssql()) |> 
  head(5) |> 
  show_query()
#> <SQL>
#> SELECT TOP 5 `df`.*
#> FROM `df`
```

If you isolate the problem to incorrect SQL generation, it would be very
helpful if you could also suggest more appropriate SQL.

## Translating individual expressions

In some cases, you might be able to track the problem down to incorrect
translation for a single column expression. In that case, you can make
your reprex even simpler with
[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md):

``` r
translate_sql(substr(x, 1, 2), con = simulate_postgres())
#> <SQL> SUBSTR(`x`, 1, 2)
translate_sql(substr(x, 1, 2), con = simulate_sqlite())
#> <SQL> SUBSTR(`x`, 1, 2)
```
