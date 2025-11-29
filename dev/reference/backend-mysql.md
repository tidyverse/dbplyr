# Backend: MySQL/MariaDB

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology. Key differences for this
backend are:

- [`paste()`](https://rdrr.io/r/base/paste.html) uses `CONCAT_WS()`

- String translations for `str_detect()`, `str_locate()`, and
  `str_replace_all()`

- Clear error message for unsupported full joins

Use `simulate_mysql()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_mysql()

simulate_mariadb()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_mysql())
lf |> transmute(x = paste0(d, " times"))
#> <SQL>
#> SELECT CONCAT_WS('', `d`, ' times') AS `x`
#> FROM `df`
```
