# Redshift backend

This backend supports Amazon Redshift databases, typically accessed via
a `RedshiftConnection` created by
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
Use `dialect_redshift()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Base translations come from [PostgreSQL
backend](https://dbplyr.tidyverse.org/dev/reference/backend-postgres.md).
There are generally few differences, apart from string manipulation.

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_redshift()

simulate_redshift()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_redshift())
lf |> transmute(x = paste(c, " times"))
#> <SQL>
#> SELECT "c" || ' ' || ' times' AS "x"
#> FROM "df"
lf |> transmute(x = substr(c, 2, 3))
#> <SQL>
#> SELECT SUBSTRING("c", 2, 2) AS "x"
#> FROM "df"
lf |> transmute(x = str_replace_all(c, "a", "z"))
#> <SQL>
#> SELECT REGEXP_REPLACE("c", 'a', 'z') AS "x"
#> FROM "df"
```
