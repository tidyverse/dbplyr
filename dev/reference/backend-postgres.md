# Backend: PostgreSQL

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology. Key differences for this
backend are:

- Many stringr functions

- lubridate date-time extraction functions

- More standard statistical summaries

Use `simulate_postgres()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_postgres()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_postgres())
lf |> summarise(x = sd(b, na.rm = TRUE))
#> <SQL>
#> SELECT STDDEV_SAMP("b") AS "x"
#> FROM "df"
lf |> summarise(y = cor(b, c), z = cov(b, c))
#> <SQL>
#> SELECT CORR("b", "c") AS "y", COVAR_SAMP("b", "c") AS "z"
#> FROM "df"
```
