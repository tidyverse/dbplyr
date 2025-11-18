# Create an sql translator

When creating a package that maps to a new SQL based src, you'll often
want to provide some additional mappings from common R commands to the
commands that your tbl provides. These three functions make that easy.

## Usage

``` r
sql_substr(f = "SUBSTR")

sql_str_sub(subset_f = "SUBSTR", length_f = "LENGTH", optional_length = TRUE)

sql_paste(default_sep, f = "CONCAT_WS")

sql_paste_infix(default_sep, op, cast)

sql_variant(
  scalar = sql_translator(),
  aggregate = sql_translator(),
  window = sql_translator()
)

sql_translator(..., .funs = list(), .parent = new.env(parent = emptyenv()))

sql_infix(f, pad = TRUE)

sql_prefix(f, n = NULL)

sql_aggregate(f, f_r = f)

sql_aggregate_2(f)

sql_aggregate_n(f, f_r = f)

sql_not_supported(f)

sql_cast(type)

sql_try_cast(type)

sql_log()

sql_cot()

sql_runif(rand_expr, n = n(), min = 0, max = 1)

base_scalar

base_agg

base_win

base_no_win

base_odbc_scalar

base_odbc_agg

base_odbc_win
```

## Arguments

- f:

  the name of the sql function as a string

- scalar, aggregate, window:

  The three families of functions than an SQL variant can supply.

- ..., .funs:

  named functions, used to add custom converters from standard R
  functions to sql functions. Specify individually in `...`, or provide
  a list of `.funs`

- .parent:

  the sql variant that this variant should inherit from. Defaults to
  `base_agg` which provides a standard set of mappings for the most
  common operators and functions.

- pad:

  If `TRUE`, the default, pad the infix operator with spaces.

- n:

  for `sql_infix()`, an optional number of arguments to expect. Will
  signal error if not correct.

- f_r:

  the name of the r function being translated as a string

## Helper functions

`sql_infix()` and `sql_prefix()` create default SQL infix and prefix
functions given the name of the SQL function. They don't perform any
input checking, but do correctly escape their input, and are useful for
quickly providing default wrappers for a new SQL variant.

## See also

[`win_over()`](https://dbplyr.tidyverse.org/dev/reference/win_over.md)
for helper functions for window functions.

[`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) for an
example of a more customised sql conversion function.

## Examples

``` r
# An example of adding some mappings for the statistical functions that
# postgresql provides: http://bit.ly/K5EdTn

postgres_agg <- sql_translator(.parent = base_agg,
  cor = sql_aggregate_2("CORR"),
  cov = sql_aggregate_2("COVAR_SAMP"),
  sd =  sql_aggregate("STDDEV_SAMP", "sd"),
  var = sql_aggregate("VAR_SAMP", "var")
)

# Next we have to simulate a connection that uses this variant
con <- simulate_dbi("TestCon")
sql_translation.TestCon <- function(x) {
  sql_variant(
    base_scalar,
    postgres_agg,
    base_no_win
  )
}

translate_sql(cor(x, y), con = con, window = FALSE)
#> Error in cor(x, y): `cor()` is not available in this SQL variant.
translate_sql(sd(income / years), con = con, window = FALSE)
#> Error in sd(income/years): `sd()` is not available in this SQL variant.

# Any functions not explicitly listed in the converter will be translated
# to sql as is, so you don't need to convert all functions.
translate_sql(regr_intercept(y, x), con = con)
#> <SQL> regr_intercept(`y`, `x`)
```
