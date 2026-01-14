# Impala backend

This backend supports Apache Impala, typically accessed via odbc. Use
`dialect_impala()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Key differences for this backend are a scattering of custom translations
provided by users, mostly focussed on bitwise operations.

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_impala()

simulate_impala()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_impala())
lf |> transmute(X = bitwNot(bitwOr(b, c)))
#> <SQL>
#> SELECT BITNOT(BITOR("b", "c")) AS "X"
#> FROM "df"
```
