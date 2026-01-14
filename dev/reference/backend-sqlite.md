# SQLite backend

This backend supports SQLite databases, typically accessed via a
`SQLiteConnection` created by
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
Use `dialect_sqlite()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Key differences for this backend are:

- Uses non-standard `LOG()` function

- Date-time extraction functions from lubridate

- Custom median translation

- Right and full joins are simulated using left joins

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_sqlite()

simulate_sqlite()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_sqlite())
lf |> transmute(x = paste(c, " times"))
#> <SQL>
#> SELECT `c` || ' ' || ' times' AS `x`
#> FROM `df`
lf |> transmute(x = log(b), y = log(b, base = 2))
#> <SQL>
#> SELECT LOG(`b`) AS `x`, LOG(`b`) / LOG(2.0) AS `y`
#> FROM `df`
```
