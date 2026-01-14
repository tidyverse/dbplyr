# Oracle backend

This backend supports Oracle databases, typically accessed via
`OraConnection` created by
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
Use `dialect_oracle()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Key differences for this backend are:

- Use `FETCH FIRST` instead of `LIMIT`

- Custom types

- [`paste()`](https://rdrr.io/r/base/paste.html) uses `||`

- Custom subquery generation (no `AS`)

- [`setdiff()`](https://generics.r-lib.org/reference/setops.html) uses
  `MINUS` instead of `EXCEPT`

Note that versions of Oracle prior to 23c have limited supported for
`TRUE` and `FALSE` and you may need to use `1` and `0` instead. See
<https://oracle-base.com/articles/23/boolean-data-type-23> for more
details.

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_oracle()

simulate_oracle()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_oracle())
lf |> transmute(x = paste0(c, " times"))
#> <SQL>
#> SELECT "c" || ' times' AS "x"
#> FROM "df"
lf |> setdiff(lf)
#> <SQL>
#> (
#>   SELECT *
#>   FROM "df"
#> )
#> MINUS
#> (
#>   SELECT *
#>   FROM "df"
#> )
```
