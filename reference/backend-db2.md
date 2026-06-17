# DB2 backend

This backend supports IBM DB2 databases, typically accessed via ODBC.
Use `dialect_db2()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/reference/tbl_lazy.md) to
see simulated SQL without connecting to a live database.

Key differences for this backend are:

- Uses `FETCH FIRST n ROWS ONLY` instead of `LIMIT n`

- Uses double quotes for identifier quoting

- [`paste()`](https://rdrr.io/r/base/paste.html) uses `||`

- DB2-specific data type names for casts (e.g. `VARCHAR(255)`, `DOUBLE`)

- Date component extraction via `YEAR()`, `MONTH()`, ..., `DAYOFYEAR()`,
  `DAYOFWEEK()`, `QUARTER()`, `WEEK()`

- `str_flatten()` uses `LISTAGG`

- Statistical summaries [`sd()`](https://rdrr.io/r/stats/sd.html),
  [`var()`](https://rdrr.io/r/stats/cor.html),
  [`cor()`](https://rdrr.io/r/stats/cor.html),
  [`cov()`](https://rdrr.io/r/stats/cor.html)

- [`runif()`](https://rdrr.io/r/stats/Uniform.html) translates to
  `RAND()`

- Regular expression functions (DB2 11.1+)

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_db2()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_db2())
lf |> head()
#> <SQL>
#> SELECT *
#> FROM "df"
#> FETCH FIRST 6 ROWS ONLY
lf |> transmute(x = paste0(d, " times"))
#> <SQL>
#> SELECT "d" || ' times' AS "x"
#> FROM "df"
lf |> summarise(x = sd(b, na.rm = TRUE))
#> <SQL>
#> SELECT STDDEV_SAMP("b") AS "x"
#> FROM "df"
```
