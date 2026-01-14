# Teradata backend

This backend supports Teradata databases, typically accessed via odbc.
Use `dialect_teradata()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Key differences for this backend are:

- Uses `TOP` instead of `LIMIT`

- Selection of user supplied translations

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_teradata()

simulate_teradata()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_teradata())
lf |> head()
#> <SQL>
#> SELECT TOP 6 "df".*
#> FROM "df"
```
