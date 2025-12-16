# SQL helpers for scalar functions

These functions help you create custom scalar SQL translations when
implementing a new backend. They are typically used within
[`sql_translator()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
to define how R functions should be translated to SQL.

- `sql_infix()` creates SQL infix operators like `+`, `-`, `*`, `/`.

- `sql_prefix()` creates SQL prefix functions like `ABS()`, `SQRT()`.

- `sql_cast()` creates SQL cast expressions like `CAST(x AS type)`.

- `sql_try_cast()` creates SQL try_cast expressions (for safe casting).

- `sql_log()` creates a SQL logarithm function with optional base.

- `sql_cot()` creates a SQL cotangent function (as `1 / TAN(x)`).

- `sql_runif()` creates a SQL expression for generating uniform random
  numbers.

## Usage

``` r
sql_infix(f, pad = TRUE)

sql_prefix(f, n = NULL)

sql_cast(type)

sql_try_cast(type)

sql_log()

sql_cot()

sql_runif(rand_expr, n = n(), min = 0, max = 1)
```

## Arguments

- f:

  The name of the SQL function as a string.

- pad:

  If `TRUE`, the default, pad the infix operator with spaces.

- n:

  For `sql_prefix()`, an optional number of arguments to expect. Will
  signal error if not correct.

- type:

  SQL type name as a string.

- rand_expr:

  An string giving an SQL expression that generates a random number
  between 0 and 1, e.g. `"RANDOM()"`.

- min, max:

  Range of random values.

## See also

Other SQL translation helpers:
[`sql_translation_agg`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md),
[`sql_translation_string`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md),
[`sql_translation_window`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md),
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
