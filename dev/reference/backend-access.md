# Backend: MS Access

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology. Key differences for this
backend are:

- `SELECT` uses `TOP`, not `LIMIT`

- Non-standard types and mathematical functions

- String concatenation uses `&`

- No `ANALYZE` equivalent

- `TRUE` and `FALSE` converted to 1 and 0

Use `simulate_access()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_access()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
lf <- lazy_frame(x = 1, y = 2, z = "a", con = simulate_access())

lf |> head()
#> <SQL>
#> SELECT TOP 6 `df`.*
#> FROM `df`
lf |> mutate(y = as.numeric(y), z = sqrt(x^2 + 10))
#> <SQL>
#> SELECT `x`, CDBL(`y`) AS `y`, SQR((`x` ^ 2.0) + 10.0) AS `z`
#> FROM `df`
lf |> mutate(a = paste0(z, " times"))
#> <SQL>
#> SELECT `df`.*, `z` & ' times' AS `a`
#> FROM `df`
```
