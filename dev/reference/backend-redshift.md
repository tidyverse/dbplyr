# Backend: Redshift

Base translations come from [PostgreSQL
backend](https://dbplyr.tidyverse.org/dev/reference/backend-postgres.md).
There are generally few differences, apart from string manipulation.

Use `simulate_redshift()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_redshift()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_redshift())
lf |> transmute(x = paste(c, " times"))
#> <SQL>
#> SELECT `c` || ' ' || ' times' AS `x`
#> FROM `df`
lf |> transmute(x = substr(c, 2, 3))
#> <SQL>
#> SELECT SUBSTRING(`c`, 2, 2) AS `x`
#> FROM `df`
lf |> transmute(x = str_replace_all(c, "a", "z"))
#> <SQL>
#> SELECT REGEXP_REPLACE(`c`, 'a', 'z') AS `x`
#> FROM `df`
```
