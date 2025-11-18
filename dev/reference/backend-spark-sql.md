# Backend: Databricks Spark SQL

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology. Key differences for this
backend are better translation of statistical aggregate functions (e.g.
[`var()`](https://rdrr.io/r/stats/cor.html),
[`median()`](https://rdrr.io/r/stats/median.html)) and use of temporary
views instead of temporary tables when copying data.

Use `simulate_spark_sql()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without converting to live access database.

## Usage

``` r
simulate_spark_sql()
```

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, d = 2, c = "z", con = simulate_spark_sql())

lf %>% summarise(x = median(d, na.rm = TRUE))
#> <SQL>
#> SELECT MEDIAN(`d`) AS `x`
#> FROM `df`
lf %>% summarise(x = var(c, na.rm = TRUE), .by = d)
#> <SQL>
#> SELECT `d`, VARIANCE(`c`) AS `x`
#> FROM `df`
#> GROUP BY `d`

lf %>% mutate(x = first(c))
#> <SQL>
#> SELECT `df`.*, FIRST_VALUE(`c`) OVER () AS `x`
#> FROM `df`
lf %>% mutate(x = first(c), .by = d)
#> <SQL>
#> SELECT `df`.*, FIRST_VALUE(`c`) OVER (PARTITION BY `d`) AS `x`
#> FROM `df`
```
