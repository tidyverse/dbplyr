# Extract a single column

This is a method for the dplyr
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html)
generic. It evaluates the query retrieving just the specified column.

## Usage

``` r
# S3 method for class 'tbl_sql'
pull(.data, var = -1, name = NULL, ...)
```

## Arguments

- .data:

  A lazy data frame backed by a database query.

- var:

  A variable specified as:

  - a literal variable name

  - a positive integer, giving the position counting from the left

  - a negative integer, giving the position counting from the right.

  The default returns the last column (on the assumption that's the
  column you've created most recently).

  This argument is taken by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names and column locations).

- name:

  An optional parameter that specifies the column to be used as names
  for a named vector. Specified in a similar manner as `var`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

## Value

A vector of data.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(x = 1:5, y = 5:1)
db |>
  mutate(z = x + y * 2) |>
  pull()
#> [1] 11 10  9  8  7
```
