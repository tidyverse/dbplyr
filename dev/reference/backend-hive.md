# Hive backend

This backend supports Apache Hive, typically accessed via odbc. Use
`dialect_hive()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Key differences for this backend are a scattering of custom translations
provided by users.

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_hive()

simulate_hive()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, d = 2, c = "z", con = dialect_hive())
lf |> transmute(x = cot(b))
#> <SQL>
#> SELECT 1.0 / TAN("b") AS "x"
#> FROM "df"
lf |> transmute(x = bitwShiftL(c, 1L))
#> <SQL>
#> SELECT SHIFTLEFT("c", 1) AS "x"
#> FROM "df"
lf |> transmute(x = str_replace_all(c, "a", "b"))
#> <SQL>
#> SELECT REGEXP_REPLACE("c", 'a', 'b') AS "x"
#> FROM "df"

lf |> summarise(x = median(d, na.rm = TRUE))
#> <SQL>
#> SELECT PERCENTILE("d", 0.5) AS "x"
#> FROM "df"
lf |> summarise(x = var(c, na.rm = TRUE))
#> <SQL>
#> SELECT VARIANCE("c") AS "x"
#> FROM "df"
```
