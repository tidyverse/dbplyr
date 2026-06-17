# Create a local lazy tibble

These functions are useful for testing SQL generation without having to
have an active database connection.

## Usage

``` r
tbl_lazy(df, con = NULL, ..., name = "df")

lazy_frame(..., con = NULL, .name = "df")
```

## Examples

``` r
library(dplyr)
df <- data.frame(x = 1, y = 2)

df_sqlite <- tbl_lazy(df, con = dialect_sqlite())
df_sqlite |> summarise(x = sd(x, na.rm = TRUE))
#> <SQL>
#> SELECT STDEV(`x`) AS `x`
#> FROM `df`
```
