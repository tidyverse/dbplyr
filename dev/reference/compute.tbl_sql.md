# Save results into a new remote table

[`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
executes the query and stores the results in a new remote table. This is
useful when you want to cache intermediate results for reuse or to
improve performance by avoiding repeated computation of complex queries.

## Usage

``` r
# S3 method for class 'tbl_sql'
compute(
  x,
  name = NULL,
  temporary = TRUE,
  overwrite = FALSE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  ...,
  cte = FALSE
)
```

## Arguments

- x:

  A lazy data frame backed by a database query.

- name:

  Name of new remote table. Use a string to create the table in the
  current catalog/schema. Use [`I()`](https://rdrr.io/r/base/AsIs.html)
  to create the table in a specific catalog/schema, e.g.
  `I("schema.table")`.

- temporary:

  if `TRUE`, will create a temporary table that is local to this
  connection and will be automatically deleted when the connection
  expires

- overwrite:

  If `TRUE`, will overwrite an existing table with name `name`. If
  `FALSE`, will throw an error if `name` already exists.

- unique_indexes:

  a list of character vectors. Each element of the list will create a
  new unique index over the specified column(s). Duplicate rows will
  result in failure.

- indexes:

  a list of character vectors. Each element of the list will create a
  new index.

- analyze:

  if `TRUE` (the default), will automatically ANALYZE the new table so
  that the query optimiser has useful information.

- ...:

  Ignored.

- cte:

  **\[experimental\]** Use common table expressions in the generated
  SQL?

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

db <- memdb_frame(a = c(3, 4, 1, 2), b = c(5, 1, 2, NA))
db |> filter(a <= 2) |> show_query()
#> <SQL>
#> SELECT `dbplyr_tmp_ZorR1u5Cjc`.*
#> FROM `dbplyr_tmp_ZorR1u5Cjc`
#> WHERE (`a` <= 2.0)
db |> filter(a <= 2) |> compute() |> show_query()
#> <SQL>
#> SELECT *
#> FROM `dbplyr_tmp_3jx2iyjisy`
```
