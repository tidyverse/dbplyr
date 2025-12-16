# Flag SQL function usage

Use `.sql$foo(x, y)` to make it clear that you're calling the SQL
`foo()` function, not the R `foo()` function. This also makes it easier
to reduce `R CMD check` notes in packages; just import `.sql` from
dbplyr with e.g. `@importFrom dbplyr .sql`.

Note that `.sql` itself does nothing and is just `NULL`; it is
automatically removed when dbplyr translates your R code to SQL.

## Usage

``` r
.sql
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- lazy_frame(x = 1, y = 2)
db |> mutate(z = .sql$CUMULATIVE_SUM(x, 1))
#> <SQL>
#> SELECT "df".*, CUMULATIVE_SUM("x", 1.0) AS "z"
#> FROM "df"
```
