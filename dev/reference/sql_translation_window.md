# SQL helpers for window functions

These functions help you create custom window SQL translations when
implementing a new backend. They are typically used within
[`sql_translator()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
to define how R window functions should be translated to SQL.

- `win_over()` makes it easy to generate the window function
  specification.

- `win_absent()`, `win_rank()`, `win_aggregate()`, and
  `win_cumulative()` provide helpers for constructing common types of
  window functions.

- `win_current_group()` and `win_current_order()` allow you to access
  the grouping and order context set up by
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  and
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).

## Usage

``` r
win_over(
  expr,
  partition = NULL,
  order = NULL,
  frame = NULL,
  con = sql_current_con()
)

win_rank(f, empty_order = FALSE)

win_aggregate(f)

win_aggregate_2(f)

win_cumulative(f)

win_absent(f)

win_current_group()

win_current_order()

win_current_frame()
```

## Arguments

- expr:

  The window expression.

- partition:

  Variables to partition over.

- order:

  Variables to order by.

- frame:

  A numeric vector of length two defining the frame.

- con:

  Database connection.

- f:

  The name of an SQL function as a string.

- empty_order:

  A logical value indicating whether to order by NULL if `order` is not
  specified.

## See also

Other SQL translation helpers:
[`sql_translation_agg`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md),
[`sql_translation_scalar`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md),
[`sql_translation_string`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md),
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)

## Examples

``` r
con <- simulate_dbi()

win_over(sql("avg(x)"), con = con)
#> <SQL> avg(x) OVER ()
win_over(sql("avg(x)"), "y", con = con)
#> <SQL> avg(x) OVER (PARTITION BY `y`)
win_over(sql("avg(x)"), order = "y", con = con)
#> <SQL> avg(x) OVER (ORDER BY `y`)
win_over(sql("avg(x)"), order = c("x", "y"), con = con)
#> <SQL> avg(x) OVER (ORDER BY `x`, `y`)
win_over(sql("avg(x)"), frame = c(-Inf, 0), order = "y", con = con)
#> <SQL> avg(x) OVER (ORDER BY `y` ROWS UNBOUNDED PRECEDING)
```
