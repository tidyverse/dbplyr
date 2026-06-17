# Combine multiple lazy queries

Combine multiple lazy queries into a single query using `UNION ALL`.
This is a convenient wrapper around `purrr::reduce(tables, union_all)`.
Like
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
(and unlike `UNION ALL`), `bind_queries()` will automatically align
columns based on their name, and fill in any missing columns with
missing values.

## Usage

``` r
bind_queries(...)
```

## Arguments

- ...:

  \<[dynamic-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Lazy tables to combine.

## Value

A lazy query.

## Examples

``` r
lf1 <- lazy_frame(x = 1, y = "a")
lf2 <- lazy_frame(x = 2, y = "b")
bind_queries(lf1, lf2)
#> <SQL>
#> SELECT *
#> FROM "df"
#> 
#> UNION ALL
#> 
#> SELECT *
#> FROM "df"

lf3 <- lazy_frame(y = "c", x = 3, z = 10)
bind_queries(lf2, lf3)
#> <SQL>
#> SELECT *, NULL AS "z"
#> FROM "df"
#> 
#> UNION ALL
#> 
#> SELECT "x", "y", "z"
#> FROM "df"

# If you already have a list, you can use splice operator
queries <- list(lf1, lf2)
bind_queries(!!!queries)
#> <SQL>
#> SELECT *
#> FROM "df"
#> 
#> UNION ALL
#> 
#> SELECT *
#> FROM "df"
```
