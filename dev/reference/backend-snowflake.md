# Backend: Snowflake

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

Use `simulate_snowflake()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_snowflake()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_snowflake())
lf |> transmute(x = paste0(d, " times"))
#> <SQL>
#> SELECT ARRAY_TO_STRING(ARRAY_CONSTRUCT_COMPACT(`d`, ' times'), '') AS `x`
#> FROM `df`
```
