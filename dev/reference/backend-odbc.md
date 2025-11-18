# Backend: ODBC

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology. Key differences for this
backend are minor translations for common data types.

Use `simulate_odbc()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_odbc()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, d = 2, c = "z", con = simulate_odbc())
lf %>% transmute(x = as.numeric(b))
#> <SQL>
#> SELECT CAST(`b` AS DOUBLE) AS `x`
#> FROM `df`
lf %>% transmute(x = as.integer(b))
#> <SQL>
#> SELECT CAST(`b` AS INT) AS `x`
#> FROM `df`
lf %>% transmute(x = as.character(b))
#> <SQL>
#> SELECT CAST(`b` AS STRING) AS `x`
#> FROM `df`
```
