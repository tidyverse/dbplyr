# Function translation

There are two parts to dbplyr SQL translation: translating dplyr verbs,
and translating expressions within those verbs. This vignette describes
how individual expressions (function calls) are translated;
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
describes how entire verbs are translated.

``` r
library(dbplyr)
library(dplyr)

con <- simulate_dbi()
```

[`dbplyr::translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
powers translation of individual function calls, and I’ll use it
extensively in this vignette to show what’s happening. You shouldn’t
need to use it ordinary code as dbplyr takes care of the translation
automatically.

``` r
translate_sql((x + y) / 2, con = con)
#> <SQL> (`x` + `y`) / 2.0
```

[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
takes an optional `con` parameter. If not supplied, this causes dbplyr
to generate (approximately) SQL-92 compliant SQL. If supplied, dbplyr
uses
[`sql_translation()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
to look up a custom environment which makes it possible for different
databases to generate slightly different SQL: see
[`vignette("new-backend")`](https://dbplyr.tidyverse.org/dev/articles/new-backend.md)
for more details. You can use the various simulate helpers to see the
translations used by different backends:

``` r
translate_sql(x ^ 2L, con = con)
#> <SQL> POWER(`x`, 2)
translate_sql(x ^ 2L, con = simulate_sqlite())
#> <SQL> POWER(`x`, 2)
translate_sql(x ^ 2L, con = simulate_access())
#> <SQL> `x` ^ 2
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
  translate_sql(1, con = con)
  #> <SQL> 1.0
  translate_sql(1L, con = con)
  #> <SQL> 1
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

## Modulo arithmetic

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
mf <- tbl_memdb(df)

df |> mutate(x %% y)
#> # A tibble: 4 × 3
#>       x     y `x%%y`
#>   <int> <int>  <int>
#> 1    10     3      1
#> 2    10    -3     -2
#> 3   -10     3      2
#> 4   -10    -3     -1
mf |> mutate(x %% y)
#> # Source:   SQL [?? x 3]
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
translate_sql(mean(x), con = con)
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.
#> <SQL> AVG(`x`) OVER ()
translate_sql(mean(x, na.rm = TRUE), con = con)
#> <SQL> AVG(`x`) OVER ()
```

Note that, by default, `translate()` assumes that the call is inside a
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) or
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and
generates a window translation. If you want to see the equivalent
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)/aggregation
translation, use `window = FALSE`:

``` r
translate_sql(mean(x, na.rm = TRUE), window = FALSE, con = con)
#> <SQL> AVG(`x`)
```

### Conditional evaluation

`if` and [`switch()`](https://rdrr.io/r/base/switch.html) are translated
to `CASE WHEN`:

``` r
translate_sql(if (x > 5) "big" else "small", con = con)
#> <SQL> CASE WHEN (`x` > 5.0) THEN 'big' WHEN NOT (`x` > 5.0) THEN 'small' END
translate_sql(switch(x, a = 1L, b = 2L, 3L), con = con)
#> <SQL> CASE `x` WHEN ('a') THEN (1) WHEN ('b') THEN (2) ELSE (3) END
```

### String manipulation

### Date/time

- string functions: `tolower`, `toupper`, `trimws`, `nchar`, `substr`
- coerce types: `as.numeric`, `as.integer`, `as.character`

## Unknown functions

Any function that dbplyr doesn’t know how to convert is left as is. This
means that database functions that are not covered by dbplyr can often
be used directly via
[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md).

### Prefix functions

Any function that dbplyr doesn’t know about will be left as is:

``` r
translate_sql(foofify(x, y), con = con)
#> <SQL> foofify(`x`, `y`)
```

Because SQL functions are generally case insensitive, I recommend using
upper case when you’re using SQL functions in R code. That makes it
easier to spot that you’re doing something unusual:

``` r
translate_sql(FOOFIFY(x, y), con = con)
#> <SQL> FOOFIFY(`x`, `y`)
```

### Infix functions

As well as prefix functions (where the name of the function comes before
the arguments), dbplyr also translates infix functions. That allows you
to use expressions like `LIKE`, which does a limited form of pattern
matching:

``` r
translate_sql(x %LIKE% "%foo%", con = con)
#> <SQL> `x` LIKE '%foo%'
```

Or use `||` for string concatenation (although most backends will
translate [`paste()`](https://rdrr.io/r/base/paste.html) and
[`paste0()`](https://rdrr.io/r/base/paste.html) for you):

``` r
translate_sql(x %||% y, con = con)
#> <SQL> `x` || `y`
```

### Special forms

SQL functions tend to have a greater variety of syntax than R. That
means there are a number of expressions that can’t be translated
directly from R code. To insert these in your own queries, you can use
literal SQL inside
[`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md):

``` r
translate_sql(sql("x!"), con = con)
#> <SQL> x!
translate_sql(x == sql("ANY VALUES(1, 2, 3)"), con = con)
#> <SQL> `x` = ANY VALUES(1, 2, 3)
```

This gives you a lot of freedom to generate the SQL you need:

``` r
mf <- memdb_frame(x = 1, y = 2)

mf |> 
  transmute(factorial = sql("x!")) |> 
  show_query()
#> <SQL>
#> SELECT x! AS `factorial`
#> FROM `dbplyr_tmp_uR8HfdVHIy`

mf |> 
  transmute(factorial = sql("CAST(x AS FLOAT)")) |> 
  show_query()
#> <SQL>
#> SELECT CAST(x AS FLOAT) AS `factorial`
#> FROM `dbplyr_tmp_uR8HfdVHIy`
```

### Error for unknown translations

If needed, you can also use the `dplyr.strict_sql` option to force
dbplyr to error if it doesn’t know how to translate a function:

``` r
options(dplyr.strict_sql = TRUE)
translate_sql(glob(x, y), con = con)
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

To see how individual window functions are translated to SQL, we can
again use
[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md):

``` r
translate_sql(mean(G), con = con)
#> <SQL> AVG(`G`) OVER ()
translate_sql(rank(G), con = con)
#> <SQL> CASE
#> WHEN (NOT((`G` IS NULL))) THEN RANK() OVER (PARTITION BY (CASE WHEN ((`G` IS NULL)) THEN 1 ELSE 0 END) ORDER BY `G`)
#> END
translate_sql(ntile(G, 2), con = con)
#> <SQL> NTILE(2) OVER (ORDER BY `G`)
translate_sql(lag(G), con = con)
#> <SQL> LAG(`G`, 1, NULL) OVER ()
```

If the tbl has been grouped or arranged previously in the pipeline, then
dplyr will use that information to set the “partition by” and “order by”
clauses. For interactive exploration, you can achieve the same effect by
setting the `vars_group` and `vars_order` arguments to
[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md):

``` r
translate_sql(cummean(G), vars_order = "year", con = con)
#> <SQL> AVG(`G`) OVER (ORDER BY `year` ROWS UNBOUNDED PRECEDING)
translate_sql(rank(), vars_group = "ID", con = con)
#> <SQL> RANK() OVER (PARTITION BY `ID`)
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
mutate(players,
  min_rank(yearID),
  order_by(yearID, cumsum(G)),
  lead(G, order_by = yearID)
)
```

Currently there is no way to order by multiple variables, except by
setting the default ordering with
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html). This
will be added in a future release.
