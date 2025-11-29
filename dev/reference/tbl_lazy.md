# Create a local lazy tibble

These functions are useful for testing SQL generation without having to
have an active database connection. See
[`simulate_dbi()`](https://dbplyr.tidyverse.org/dev/reference/simulate_dbi.md)
for a list available database simulations.

## Usage

``` r
tbl_lazy(df, con = NULL, ..., name = "df")

lazy_frame(..., con = NULL, .name = "df")
```

## Examples

``` r
library(dplyr)
df <- data.frame(x = 1, y = 2)

df_sqlite <- tbl_lazy(df, con = simulate_sqlite())
df_sqlite |> summarise(x = sd(x, na.rm = TRUE)) |> show_query()
#> <SQL>
#> SELECT STDEV(`x`) AS `x`
#> FROM `df`
```
