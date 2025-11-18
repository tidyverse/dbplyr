# Backend: SAP HANA

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology. Key differences for this
backend are:

- Temporary tables get `#` prefix and use `LOCAL TEMPORARY COLUMN`.

- No table analysis performed in
  [`dplyr::copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html).

- [`paste()`](https://rdrr.io/r/base/paste.html) uses `||`

- Note that you can't create new boolean columns from logical
  expressions; you need to wrap with explicit `ifelse`:
  `ifelse(x > y, TRUE, FALSE)`.

Use `simulate_hana()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_hana()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_hana())
lf %>% transmute(x = paste0(d, " times"))
#> <SQL>
#> SELECT `d` || ' times' AS `x`
#> FROM `df`
```
