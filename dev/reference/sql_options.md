# Options for generating SQL

Options for generating SQL

## Usage

``` r
sql_options(cte = FALSE, use_star = TRUE, qualify_all_columns = FALSE)
```

## Arguments

- cte:

  If `FALSE`, the default, subqueries are used. If `TRUE` common table
  expressions are used.

- use_star:

  If `TRUE`, the default, `*` is used to select all columns of a table.
  If `FALSE` all columns are explicitly selected.

- qualify_all_columns:

  If `FALSE`, the default, columns are only qualified with the table
  they come from if the same column name appears in multiple tables.

## Value

A \<dbplyr_sql_options\> object.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
lf1 <- lazy_frame(key = 1, a = 1, b = 2)
lf2 <- lazy_frame(key = 1, a = 1, c = 3)

result <- left_join(lf1, lf2, by = "key") |>
  filter(c >= 3)

show_query(result)
#> <SQL>
#> SELECT
#>   "df_LHS"."key" AS "key",
#>   "df_LHS"."a" AS "a.x",
#>   "b",
#>   "df_RHS"."a" AS "a.y",
#>   "c"
#> FROM "df" AS "df_LHS"
#> LEFT JOIN "df" AS "df_RHS"
#>   ON ("df_LHS"."key" = "df_RHS"."key")
#> WHERE ("df_RHS"."c" >= 3.0)
sql_options <- sql_options(cte = TRUE, qualify_all_columns = TRUE)
show_query(result, sql_options = sql_options)
#> <SQL>
#> SELECT
#>   "df_LHS"."key" AS "key",
#>   "df_LHS"."a" AS "a.x",
#>   "df_LHS"."b" AS "b",
#>   "df_RHS"."a" AS "a.y",
#>   "df_RHS"."c" AS "c"
#> FROM "df" AS "df_LHS"
#> LEFT JOIN "df" AS "df_RHS"
#>   ON ("df_LHS"."key" = "df_RHS"."key")
#> WHERE ("df_RHS"."c" >= 3.0)
```
