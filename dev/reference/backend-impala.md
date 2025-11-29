# Backend: Impala

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology. Key differences for this
backend are a scattering of custom translations provided by users,
mostly focussed on bitwise operations.

Use `simulate_impala()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_impala()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_impala())
lf |> transmute(X = bitwNot(bitwOr(b, c)))
#> <SQL>
#> SELECT BITNOT(BITOR(`b`, `c`)) AS `X`
#> FROM `df`
```
