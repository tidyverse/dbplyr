# MySQL/MariaDB backend

This backend supports MySQL and MariaDB databases, typically accessed
via `MySQLConnection` or `MariaDBConnection` created by
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
Use `dialect_mysql()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Key differences for this backend are:

- [`paste()`](https://rdrr.io/r/base/paste.html) uses `CONCAT_WS()`

- String translations for `str_detect()`, `str_locate()`, and
  `str_replace_all()`

- Clear error message for unsupported full joins

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_mariadb()

dialect_mysql()

simulate_mysql()

simulate_mariadb()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_mysql())
lf |> transmute(x = paste0(d, " times"))
#> <SQL>
#> SELECT CONCAT_WS('', `d`, ' times') AS `x`
#> FROM `df`
```
