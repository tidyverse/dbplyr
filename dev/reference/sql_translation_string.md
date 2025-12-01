# SQL helpers for string functions

These functions help you create custom string SQL translations when
implementing a new backend. They are typically used within
[`sql_translator()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
to define how R string functions should be translated to SQL.

- `sql_substr()` creates a SQL substring function translator that
  converts R's `substr(x, start, stop)` to SQL's
  `SUBSTR(x, start, length)`.

- `sql_str_sub()` creates a SQL substring function translator that
  handles stringr's `str_sub()` with support for negative indices.

- `sql_paste()` creates a SQL paste function using `CONCAT_WS()` or
  similar.

- `sql_paste_infix()` creates a SQL paste function using an infix
  operator like `||`.

## Usage

``` r
sql_substr(f = "SUBSTR")

sql_str_sub(subset_f = "SUBSTR", length_f = "LENGTH", optional_length = TRUE)

sql_paste(default_sep, f = "CONCAT_WS")

sql_paste_infix(default_sep, op, cast)
```

## Arguments

- f:

  The name of the SQL function as a string.

- subset_f:

  The name of the SQL substring function.

- length_f:

  The name of the SQL string length function.

- optional_length:

  Whether the length argument is optional in the SQL substring function.

- default_sep:

  The default separator for paste operations.

- op:

  The SQL operator to use for infix paste operations.

- cast:

  A function to cast values to strings.

## See also

Other SQL translation helpers:
[`sql_translation_agg`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md),
[`sql_translation_scalar`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md),
[`sql_translation_window`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md),
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
