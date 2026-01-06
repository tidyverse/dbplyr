# Translate an expression to SQL

dbplyr translates commonly used base functions including logical (`!`,
`&`, `|`), arithmetic (`^`), and comparison (`!=`) operators, as well as
common summary ([`mean()`](https://rdrr.io/r/base/mean.html),
[`var()`](https://rdrr.io/r/stats/cor.html)), and transformation
([`log()`](https://rdrr.io/r/base/Log.html)) functions. All other
functions will be preserved as is. R's infix functions (e.g. `%like%`)
will be converted to their SQL equivalents (e.g. `LIKE`).

Learn more in
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md).

## Usage

``` r
translate_sql(
  ...,
  con,
  vars_group = NULL,
  vars_order = NULL,
  vars_frame = NULL,
  window = TRUE
)

translate_sql_(
  dots,
  con,
  vars_group = NULL,
  vars_order = NULL,
  vars_frame = NULL,
  window = TRUE,
  context = list()
)
```

## Arguments

- ..., dots:

  Expressions to translate. `translate_sql()` automatically quotes them
  for you. `translate_sql_()` expects a list of expression objects.

- con:

  Database connection used to determine the SQL dialect.

- vars_group, vars_order, vars_frame:

  Parameters used in the `OVER` expression of windowed functions.

- window:

  Use `FALSE` to suppress generation of the `OVER` statement used for
  window functions. This is necessary when generating SQL for a grouped
  summary.

- context:

  Use to carry information for special translation cases. For example,
  MS SQL needs a different conversion for is.na() in WHERE vs. SELECT
  clauses. Expects a list.

## Examples

``` r
con <- simulate_dbi()

# Regular maths is translated in a very straightforward way
translate_sql(x + 1, con = con)
#> <SQL> "x" + 1.0
translate_sql(sin(x) + tan(y), con = con)
#> <SQL> SIN("x") + TAN("y")

# Note that all variable names are escaped
translate_sql(like == "x", con = con)
#> <SQL> "like" = 'x'
# In ANSI SQL: "" quotes variable _names_, '' quotes strings

# Logical operators are converted to their sql equivalents
translate_sql(x < 5 & !(y >= 5), con = con)
#> <SQL> "x" < 5.0 AND NOT(("y" >= 5.0))
# xor() doesn't have a direct SQL equivalent
translate_sql(xor(x, y), con = con)
#> <SQL> "x" OR "y" AND NOT ("x" AND "y")

# If is translated into case when
translate_sql(if (x > 5) "big" else "small", con = con)
#> <SQL> CASE WHEN ("x" > 5.0) THEN 'big' WHEN NOT ("x" > 5.0) THEN 'small' END

# Infix functions are passed onto SQL with % removed
translate_sql(first %like% "Had%", con = con)
#> <SQL> "first" like 'Had%'
translate_sql(first %is% NA, con = con)
#> <SQL> "first" is NULL
translate_sql(first %in% c("John", "Roger", "Robert"), con = con)
#> <SQL> "first" IN ('John', 'Roger', 'Robert')

# And be careful if you really want integers
translate_sql(x == 1, con = con)
#> <SQL> "x" = 1.0
translate_sql(x == 1L, con = con)
#> <SQL> "x" = 1

# If you have an already quoted object, use translate_sql_:
x <- quote(y + 1 / sin(t))
translate_sql_(list(x), con = simulate_dbi())
#> <SQL> "y" + 1.0 / SIN("t")

# Windowed translation --------------------------------------------
# Known window functions automatically get OVER()
translate_sql(mpg > mean(mpg), con = con)
#> <SQL> "mpg" > AVG("mpg") OVER ()

# Suppress this with window = FALSE
translate_sql(mpg > mean(mpg), window = FALSE, con = con)
#> <SQL> "mpg" > AVG("mpg")

# vars_group controls partition:
translate_sql(mpg > mean(mpg), vars_group = "cyl", con = con)
#> <SQL> "mpg" > AVG("mpg") OVER (PARTITION BY "cyl")

# and vars_order controls ordering for those functions that need it
translate_sql(cumsum(mpg), con = con)
#> Warning: Windowed expression `SUM("mpg")` does not have explicit order.
#> ℹ Please use `arrange()`, `window_order()`, or `.order` to make
#>   deterministic.
#> <SQL> SUM("mpg") OVER (ROWS UNBOUNDED PRECEDING)
translate_sql(cumsum(mpg), vars_order = "mpg", con = con)
#> <SQL> SUM("mpg") OVER (ORDER BY "mpg" ROWS UNBOUNDED PRECEDING)
```
