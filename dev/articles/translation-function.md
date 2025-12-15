# Function translation

There are two parts to dbplyr SQL translation: translating dplyr verbs,
and translating expressions within those verbs. This vignette describes
how individual expressions (function calls) are translated;
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
describes how entire verbs are translated.

``` r
library(dbplyr)
library(dplyr, warn.conflicts = FALSE)
```

## Getting started with translations

In this vignette, I’ll use
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to create a toy lazy table that allows us to see the translation without
needing to connect to a real database:

``` r
lf <- lazy_frame(x = 1, y = 2, g = "a")
lf |> mutate(z = (x + y) / 2)
#> <SQL>
#> SELECT `df`.*, (`x` + `y`) / 2.0 AS `z`
#> FROM `df`
```

The default
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
uses a generic database that generates (approximately) SQL-92 compliant
SQL. You can use `simulate_*()` connections to see the translations used
by different backends. Different databases generate slightly different
SQL; see
[`vignette("new-backend")`](https://dbplyr.tidyverse.org/dev/articles/new-backend.md)
for more details.

``` r
lf_sqlite <- lazy_frame(x = 1, con = simulate_sqlite())
lf_access <- lazy_frame(x = 1, con = simulate_access())

lf_sqlite |> transmute(z = x^2)
#> <SQL>
#> SELECT POWER(`x`, 2.0) AS `z`
#> FROM `df`
lf_access |> transmute(z = x^2)
#> <SQL>
#> SELECT `x` ^ 2.0 AS `z`
#> FROM `df`
```

Perfect translation is not possible because databases don’t have all the
functions that R does. The goal of dbplyr is to provide a semantic
rather than a literal translation: what you mean, rather than precisely
what is done. In fact, even for functions that exist both in databases
and in R, you shouldn’t expect results to be identical; database
programmers have different priorities than R core programmers. For
example, in R in order to get a higher level of numerical accuracy,
[`mean()`](https://rdrr.io/r/base/mean.html) loops through the data
twice. R’s [`mean()`](https://rdrr.io/r/base/mean.html) also provides a
`trim` option for computing trimmed means; this is something that
databases do not provide.

If you’re interested in how
[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
is implemented, the basic techniques that underlie the implementation of
[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
are described in [“Advanced
R”](https://adv-r.hadley.nz/translation.html).

## Basic differences

There are two fundamental differences between R and SQL:

- `"` and `'` mean different things. R can use either `"` or `'` for
  strings, but in ANSI SQL, `"` is used for names and only `'` can be
  used for strings.

- R and SQL have different defaults for integers and reals. In R, 1 is a
  real, and 1L is an integer. In SQL, 1 is an integer, and 1.0 is a
  real.

  ``` r
  lf |> transmute(z = 1)
  #> <SQL>
  #> SELECT 1.0 AS `z`
  #> FROM `df`
  lf |> transmute(z = 1L)
  #> <SQL>
  #> SELECT 1 AS `z`
  #> FROM `df`
  ```

## Known functions

### Mathematics

- basic math operators: `+`, `-`, `*`, `/`, `^`
- trigonometry: [`acos()`](https://rdrr.io/r/base/Trig.html),
  [`asin()`](https://rdrr.io/r/base/Trig.html),
  [`atan()`](https://rdrr.io/r/base/Trig.html),
  [`atan2()`](https://rdrr.io/r/base/Trig.html),
  [`cos()`](https://rdrr.io/r/base/Trig.html), `cot()`,
  [`tan()`](https://rdrr.io/r/base/Trig.html),
  [`sin()`](https://rdrr.io/r/base/Trig.html)
- hypergeometric: [`cosh()`](https://rdrr.io/r/base/Hyperbolic.html),
  `coth()`, [`sinh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`tanh()`](https://rdrr.io/r/base/Hyperbolic.html)
- logarithmic: [`log()`](https://rdrr.io/r/base/Log.html),
  [`log10()`](https://rdrr.io/r/base/Log.html),
  [`exp()`](https://rdrr.io/r/base/Log.html)
- misc: [`abs()`](https://rdrr.io/r/base/MathFun.html),
  [`ceiling()`](https://rdrr.io/r/base/Round.html),
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
  [`sign()`](https://rdrr.io/r/base/sign.html),
  [`round()`](https://rdrr.io/r/base/Round.html)

### Modulo arithmetic

dbplyr translates `%%` to the SQL equivalents but note that it’s not
precisely the same: most databases use truncated division where the
modulo operator takes the sign of the dividend, where R using the
mathematically preferred floored division with the modulo sign taking
the sign of the divisor.

``` r
df <- tibble(
  x = c(10L, 10L, -10L, -10L), 
  y = c(3L, -3L, 3L, -3L)
)
db <- tbl_memdb(df)

df |> mutate(x %% y)
#> # A tibble: 4 × 3
#>       x     y `x%%y`
#>   <int> <int>  <int>
#> 1    10     3      1
#> 2    10    -3     -2
#> 3   -10     3      2
#> 4   -10    -3     -1
db |> mutate(x %% y)
#> # A query:  ?? x 3
#> # Database: sqlite 3.51.1 [:memory:]
#>       x     y `x%%y`
#>   <int> <int>  <int>
#> 1    10     3      1
#> 2    10    -3      1
#> 3   -10     3     -1
#> 4   -10    -3     -1
```

dbplyr no longer translates `%/%` because there’s no robust
cross-database translation available.

### Logical comparisons

- logical comparisons: `<`, `<=`, `!=`, `>=`, `>`, `==`, `%in%`
- boolean operations: `&`, `&&`, `|`, `||`, `!`,
  [`xor()`](https://rdrr.io/r/base/Logic.html)

### Aggregation

All databases provide translation for the basic aggregations:
[`mean()`](https://rdrr.io/r/base/mean.html),
[`sum()`](https://rdrr.io/r/base/sum.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html),
[`sd()`](https://rdrr.io/r/stats/sd.html),
[`var()`](https://rdrr.io/r/stats/cor.html). Databases automatically
drop NULLs (their equivalent of missing values) whereas in R you have to
ask nicely. The aggregation functions warn you about this important
difference:

``` r
lf |> summarise(z = mean(x))
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.
#> <SQL>
#> SELECT AVG(`x`) AS `z`
#> FROM `df`
lf |> summarise(z = mean(x, na.rm = TRUE))
#> <SQL>
#> SELECT AVG(`x`) AS `z`
#> FROM `df`
```

Note that aggregation functions used inside
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) or
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) generate
a window translation:

``` r
lf |> mutate(z = mean(x, na.rm = TRUE))
#> <SQL>
#> SELECT `df`.*, AVG(`x`) OVER () AS `z`
#> FROM `df`
lf |> filter(mean(x, na.rm = TRUE) > 0)
#> <SQL>
#> SELECT `x`, `y`, `g`
#> FROM (
#>   SELECT `df`.*, AVG(`x`) OVER () AS `col01`
#>   FROM `df`
#> ) AS `q01`
#> WHERE (`col01` > 0.0)
```

### Conditional evaluation

`if` and [`switch()`](https://rdrr.io/r/base/switch.html) are translated
to `CASE WHEN`:

``` r
lf |> mutate(z = if (x > 5) "big" else "small")
#> <SQL>
#> SELECT
#>   `df`.*,
#>   CASE WHEN (`x` > 5.0) THEN 'big' WHEN NOT (`x` > 5.0) THEN 'small' END AS `z`
#> FROM `df`
lf |> mutate(z = switch(g, a = 1L, b = 2L, 3L))
#> <SQL>
#> SELECT
#>   `df`.*,
#>   CASE `g` WHEN ('a') THEN (1) WHEN ('b') THEN (2) ELSE (3) END AS `z`
#> FROM `df`
```

### Date/time

- string functions: `tolower`, `toupper`, `trimws`, `nchar`, `substr`
- coerce types: `as.numeric`, `as.integer`, `as.character`

## Unknown functions

Any function that dbplyr doesn’t know how to convert is left as is. This
means that database functions that are not covered by dbplyr can often
be used directly.

### Prefix functions

Any function that dbplyr doesn’t know about will be left as is:

``` r
lf |> mutate(z = foofify(x, y))
#> <SQL>
#> SELECT `df`.*, foofify(`x`, `y`) AS `z`
#> FROM `df`
```

But to make it clear that you’re deliberately calling a SQL function, we
recommend using the `.sql` pronoun:

``` r
lf |> transmute(z = .sql$foofify(x, y))
#> <SQL>
#> SELECT foofify(`x`, `y`) AS `z`
#> FROM `df`
```

If you’re working inside a package, this also makes it easier to avoid
`R CMD CHECK` notes. Just import `.sql` from dbplyr using a roxygen2 tag
like `@importFrom dbplyr .sql`

### Infix functions

As well as prefix functions (where the name of the function comes before
the arguments), dbplyr also translates infix functions. That allows you
to use expressions like `LIKE`, which does a limited form of pattern
matching:

``` r
lf |> filter(x %LIKE% "%foo%")
#> <SQL>
#> SELECT `df`.*
#> FROM `df`
#> WHERE (`x` LIKE '%foo%')
```

You can also use `str_like()` for this common case:

``` r
lf |> filter(str_like(x, "%foo%"))
#> <SQL>
#> SELECT `df`.*
#> FROM `df`
#> WHERE (`x` LIKE '%foo%')
```

You could use `%||%` for string concatenation, but in most cases it’s
more R-like to use [`paste()`](https://rdrr.io/r/base/paste.html) or
[`paste0()`](https://rdrr.io/r/base/paste.html):

``` r
lf |> transmute(z = x %||% y)
#> <SQL>
#> SELECT `x` || `y` AS `z`
#> FROM `df`
lf |> transmute(z = paste0(x, y))
#> <SQL>
#> SELECT CONCAT_WS('', `x`, `y`) AS `z`
#> FROM `df`
lf |> transmute(z = paste(x, y))
#> <SQL>
#> SELECT CONCAT_WS(' ', `x`, `y`) AS `z`
#> FROM `df`
```

### Special forms

SQL functions tend to have a greater variety of syntax than R. That
means there are a number of expressions that can’t be translated
directly from R code. To insert these in your own queries, you can use
literal SQL inside
[`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md):

``` r
lf |> transmute(z = sql("x!"))
#> <SQL>
#> SELECT x! AS `z`
#> FROM `df`
lf |> transmute(z = x == sql("ANY VALUES(1, 2, 3)"))
#> <SQL>
#> SELECT `x` = ANY VALUES(1, 2, 3) AS `z`
#> FROM `df`
```

This gives you a lot of freedom to generate the SQL you need:

``` r
lf |> transmute(factorial = sql("x!"))
#> <SQL>
#> SELECT x! AS `factorial`
#> FROM `df`
lf |> transmute(factorial = sql("CAST(x AS FLOAT)"))
#> <SQL>
#> SELECT CAST(x AS FLOAT) AS `factorial`
#> FROM `df`
```

### Error for unknown translations

If needed, you can also use the `dplyr.strict_sql` option to force
dbplyr to error if it doesn’t know how to translate a function:

``` r
options(dplyr.strict_sql = TRUE)
lf |> mutate(z = glob(x, y))
#> Error in `glob()`:
#> ! Don't know how to translate `glob()`
```

## Window functions

Things get a little trickier with window functions, because SQL’s window
functions are considerably more expressive than the specific variants
provided by base R or dplyr. They have the form
`[expression] OVER ([partition clause] [order clause] [frame_clause])`:

- The **expression** is a combination of variable names and window
  functions. Support for window functions varies from database to
  database, but most support the ranking functions, `lead`, `lag`,
  `nth`, `first`, `last`, `count`, `min`, `max`, `sum`, `avg` and
  `stddev`.

- The **partition clause** specifies how the window function is broken
  down over groups. It plays an analogous role to `GROUP BY` for
  aggregate functions, and
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) in
  dplyr. It is possible for different window functions to be partitioned
  into different groups, but not all databases support it, and neither
  does dplyr.

- The **order clause** controls the ordering (when it makes a
  difference). This is important for the ranking functions since it
  specifies which variables to rank by, but it’s also needed for
  cumulative functions and lead. Whenever you’re thinking about before
  and after in SQL, you must always tell it which variable defines the
  order. If the order clause is missing when needed, some databases fail
  with an error message while others return non-deterministic results.

- The **frame clause** defines which rows, or **frame**, that are passed
  to the window function, describing which rows (relative to the current
  row) should be included. The frame clause provides two offsets which
  determine the start and end of frame. There are three special values:
  -Inf means to include all preceding rows (in SQL, “unbounded
  preceding”), 0 means the current row (“current row”), and Inf means
  all following rows (“unbounded following”). The complete set of
  options is comprehensive, but fairly confusing, and is summarised
  visually below.

  ![A visual summary of the frame clause using the real line labelled
  with negative infinity, -3, -2, -1, 0, 1, 2, 3, infinity. The most
  important clauses are rolling, cumulative, and recycling. Rolling,
  e.g. between 1 preceding and 1, following, run from -1 to -1.
  Cumulative, between unbounded preceding and current row, runs from
  negative infinity to 0. Recycled, between unbound preceeding and
  unbound following, runs from negative infinity to positive
  infinity.](windows.png)

  Of the many possible specifications, only three are commonly used.
  They select between aggregation variants:

  - Recycled: `BETWEEN UNBOUND PRECEDING AND UNBOUND FOLLOWING`

  - Cumulative: `BETWEEN UNBOUND PRECEDING AND CURRENT ROW`

  - Rolling: `BETWEEN 2 PRECEDING AND 2 FOLLOWING`

  dbplyr generates the frame clause based on whether you’re using a
  recycled aggregate or a cumulative aggregate.

To see how individual window functions are translated to SQL, we can use
[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html):

``` r
lf <- lazy_frame(g = 1, year = 2020, id = 3, con = simulate_dbi())

lf |> transmute(
  mean = mean(g), 
  rank = min_rank(g), 
  cumsum = cumsum(g),
  lag = lag(g)
)
#> Warning: Windowed expression `SUM(`g`)` does not have explicit order.
#> ℹ Please use `arrange()` or `window_order()` to make deterministic.
#> <SQL>
#> SELECT
#>   AVG(`g`) OVER () AS `mean`,
#>   CASE
#> WHEN (NOT((`g` IS NULL))) THEN RANK() OVER (PARTITION BY (CASE WHEN ((`g` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `g`)
#> END AS `rank`,
#>   SUM(`g`) OVER (ROWS UNBOUNDED PRECEDING) AS `cumsum`,
#>   LAG(`g`, 1, NULL) OVER () AS `lag`
#> FROM `df`
```

If the lazy frame has been grouped or arranged previously in the
pipeline, then dbplyr will use that information to set the “partition
by” and “order by” clauses:

``` r
lf |> arrange(year) |> mutate(z = cummean(g))
#> <SQL>
#> SELECT `df`.*, AVG(`g`) OVER (ORDER BY `year` ROWS UNBOUNDED PRECEDING) AS `z`
#> FROM `df`
#> ORDER BY `year`
lf |> group_by(id) |> mutate(z = rank())
#> <SQL>
#> SELECT `df`.*, RANK() OVER (PARTITION BY `id`) AS `z`
#> FROM `df`
```

There are some challenges when translating window functions between R
and SQL, because dbplyr tries to keep the window functions as similar as
possible to both the existing R analogues and to the SQL functions. This
means that there are three ways to control the order clause depending on
which window function you’re using:

- For ranking functions, the ordering variable is the first argument:
  `rank(x)`, `ntile(y, 2)`. If omitted or `NULL`, will use the default
  ordering associated with the tbl (as set by
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)).

- Accumulating aggregates only take a single argument (the vector to
  aggregate). To control ordering, use
  [`order_by()`](https://dplyr.tidyverse.org/reference/order_by.html).

- Aggregates implemented in dplyr (`lead`, `lag`, `nth_value`,
  `first_value`, `last_value`) have an `order_by` argument. Supply it to
  override the default ordering.

The three options are illustrated in the snippet below:

``` r
lf |> transmute(
  x1 = min_rank(g),
  x2 = order_by(year, cumsum(g)),
  x3 = lead(g, order_by = year)
)
#> <SQL>
#> SELECT
#>   CASE
#> WHEN (NOT((`g` IS NULL))) THEN RANK() OVER (PARTITION BY (CASE WHEN ((`g` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `g`)
#> END AS `x1`,
#>   SUM(`g`) OVER (ORDER BY `year` ROWS UNBOUNDED PRECEDING) AS `x2`,
#>   LEAD(`g`, 1, NULL) OVER (ORDER BY `year`) AS `x3`
#> FROM `df`
```

Currently there is no way to order by multiple variables, except by
setting the default ordering with
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html). This
will be added in a future release.
