# ANSI SQL backend

This is the base dialect for ANSI compliant SQL, forming the foundation
of all other dialects. Use `dialect_ansi()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
for a list of functions that are translated.

## Usage

``` r
dialect_ansi()

simulate_dbi(class = character(), ...)
```

## Arguments

- class, ...:

  No longer used.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_ansi())
lf |> transmute(x = mean(b, na.rm = TRUE))
#> <SQL>
#> SELECT AVG("b") OVER () AS "x"
#> FROM "df"
lf |> transmute(x = log(b), y = log(b, base = 2))
#> <SQL>
#> SELECT LN("b") AS "x", LOG(2.0, "b") AS "y"
#> FROM "df"
```
