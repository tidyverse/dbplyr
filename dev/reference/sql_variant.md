# Create an SQL translator

`sql_variant()` creates a SQL variant, a list of translators for scalar,
aggregate, and window functions. `sql_translator()` creates a
translator, an environment containing R to SQL translations. When
creating a backend, you'll use these functions to customize how R
functions are converted to SQL.

## Usage

``` r
sql_variant(
  scalar = sql_translator(),
  aggregate = sql_translator(),
  window = sql_translator()
)

sql_translator(..., .funs = list(), .parent = new.env(parent = emptyenv()))

base_scalar

base_agg

base_win

base_no_win

base_odbc_scalar

base_odbc_agg

base_odbc_win
```

## Arguments

- scalar, aggregate, window:

  The three families of functions that an SQL variant can supply.

- ..., .funs:

  Named functions, used to add custom converters from standard R
  functions to SQL functions. Specify individually in `...`, or provide
  a list of `.funs`.

- .parent:

  The SQL variant that this variant should inherit from. Defaults to
  `base_agg` which provides a standard set of mappings for the most
  common operators and functions.

## Base translators

dbplyr provides the following base translators that implement standard
SQL semantics:

- `base_scalar` - scalar functions and operators

- `base_agg` - aggregate functions

- `base_win` - window functions

- `base_no_win` - versions of window functions that throw errors

## See also

Other SQL translation helpers:
[`sql_translation_agg`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md),
[`sql_translation_scalar`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md),
[`sql_translation_string`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md),
[`sql_translation_window`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)

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
