# SQL helpers for aggregate functions

These functions help you create custom aggregate SQL translations when
implementing a new backend. They are typically used within
[`sql_translator()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
to define how R aggregate functions should be translated to SQL.

- `sql_aggregate()` creates a SQL aggregate function translator for
  functions with a single argument and an optional `na.rm` parameter
  (e.g., `SUM()`, `AVG()`).

- `sql_aggregate_2()` creates a SQL aggregate function translator for
  functions with exactly two arguments (e.g., `CORR()`, `COVAR_SAMP()`).

- `sql_aggregate_n()` creates a SQL aggregate function translator for
  functions with any number of arguments and an optional `na.rm`
  parameter (e.g., `LEAST()`, `GREATEST()`).

- `sql_not_supported()` creates a function that throws an informative
  error when a function is not supported in SQL.

- `sql_check_na_rm()` is a helper that you can use in aggregate
  functions to direct the user towards setting `na.rm = TRUE`.

## Usage

``` r
sql_aggregate(f, f_r = f)

sql_aggregate_2(f)

sql_aggregate_n(f, f_r = f)

sql_check_na_rm(na.rm)

sql_not_supported(f)
```

## Arguments

- f:

  The name of the SQL function as a string.

- f_r:

  The name of the R function being translated as a string.

- na.rm:

  Logical indicating whether missing values should be removed. In SQL,
  missing values are always removed in aggregate functions, so this
  function will warn if `na.rm` is not `TRUE`.

## See also

Other SQL translation helpers:
[`sql_translation_scalar`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md),
[`sql_translation_string`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md),
[`sql_translation_window`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md),
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
