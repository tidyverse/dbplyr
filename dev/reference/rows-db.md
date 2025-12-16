# Edit individual rows in the underlying database table

These are methods for the dplyr
[`dplyr::rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_append()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html),
and
[`dplyr::rows_delete()`](https://dplyr.tidyverse.org/reference/rows.html)
generics.

When `in_place = TRUE` these verbs do not generate `SELECT` queries, but
instead directly modify the underlying data using `INSERT`, `UPDATE`, or
`DELETE` operators. This will require that you have write access to the
database: the connection needs permission to insert, modify or delete
rows, but not to alter the structure of the table.

The default, `in_place = FALSE`, generates equivalent lazy tables (using
`SELECT` queries) that allow previewing the result without actually
modifying the underlying table on the database.

## Usage

``` r
# S3 method for class 'tbl_lazy'
rows_insert(
  x,
  y,
  by = NULL,
  ...,
  conflict = c("error", "ignore"),
  copy = "none",
  in_place = FALSE,
  returning = NULL,
  method = NULL
)

# S3 method for class 'tbl_lazy'
rows_append(x, y, ..., copy = "none", in_place = FALSE, returning = NULL)

# S3 method for class 'tbl_lazy'
rows_update(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = "none",
  in_place = FALSE,
  returning = NULL
)

# S3 method for class 'tbl_lazy'
rows_patch(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = "none",
  in_place = FALSE,
  returning = NULL
)

# S3 method for class 'tbl_lazy'
rows_upsert(
  x,
  y,
  by = NULL,
  ...,
  copy = "none",
  in_place = FALSE,
  returning = NULL,
  method = NULL
)

# S3 method for class 'tbl_lazy'
rows_delete(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = "none",
  in_place = FALSE,
  returning = NULL
)
```

## Arguments

- x:

  A lazy table. For `in_place = TRUE`, this must be a table instantiated
  with [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
  or
  [`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html),
  not to a lazy query. The
  [`remote_name()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  function is used to determine the name of the table to be updated.

- y:

  A lazy table, data frame, or data frame extensions (e.g. a tibble).

- by:

  An unnamed character vector giving the key columns. The key columns
  must exist in both `x` and `y`. Keys typically uniquely identify each
  row, but this is only enforced for the key values of `y` when
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html), or
  [`rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html) are
  used.

  By default, we use the first column in `y`, since the first column is
  a reasonable place to put an identifier variable.

- ...:

  Other parameters passed onto methods.

- conflict:

  For
  [`rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html),
  how should keys in `y` that conflict with keys in `x` be handled? A
  conflict arises if there is a key in `y` that already exists in `x`.

  One of:

  - `"error"`, the default, is not supported for database tables. To get
    the same behaviour add a unique index on the `by` columns and use
    [`rows_append()`](https://dplyr.tidyverse.org/reference/rows.html).

  - `"ignore"` will ignore rows in `y` with keys that conflict with keys
    in `x`.

- copy:

  If `x` and `y` are not from the same data source, `copy` controls how
  `y` is copied into the same source as `x`. There are three options:

  - `"none"`, the default, will error if `y` needs to be copied. This
    ensures that you don't accidentally copy large datasets from R to
    the database.

  - `"temp-table"`: copies `y` into a temporary table in the same
    database as `x`. `*_join()` will automatically run `ANALYZE` on the
    created table in the hope that this will make your queries as
    efficient as possible by giving more data to the query planner.

  - `"inline"`: `y` will be inlined into the query using
    [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md).
    This is should faster for small datasets and doesn't require write
    access.

  `TRUE` (`"temp-table"`) and `FALSE` (`"none"`) are also accepted for
  backward compatibility.

- in_place:

  Should `x` be modified in place? If `FALSE` will generate a `SELECT`
  query that returns the modified table; if `TRUE` will modify the
  underlying table using a DML operation (`INSERT`, `UPDATE`, `DELETE`
  or similar).

- returning:

  Columns to return. See
  [`get_returned_rows()`](https://dbplyr.tidyverse.org/dev/reference/get_returned_rows.md)
  for details.

- method:

  A string specifying the method to use. This is only relevant for
  `in_place = TRUE`.

- unmatched:

  For
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html), and
  [`rows_delete()`](https://dplyr.tidyverse.org/reference/rows.html),
  how should keys in `y` that are unmatched by the keys in `x` be
  handled?

  One of:

  - `"error"`, the default, is not supported for database tables. Add a
    foreign key constraint on the `by` columns of `y` to let the
    database check this behaviour for you.

  - `"ignore"` will ignore rows in `y` with keys that are unmatched by
    the keys in `x`.

## Value

A new `tbl_lazy` of the modified data. With `in_place = FALSE`, the
result is a lazy query that prints visibly, because the purpose of this
operation is to preview the results. With `in_place = TRUE`, `x` is
returned invisibly, because the purpose of this operation is the side
effect of modifying rows in the table behind `x`.

## Examples

``` r
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbExecute(con, "CREATE TABLE Ponies (
   id INTEGER PRIMARY KEY AUTOINCREMENT,
   name TEXT,
   cutie_mark TEXT
)")
#> [1] 0

ponies <- tbl(con, "Ponies")

applejack <- copy_inline(con, data.frame(
  name = "Apple Jack",
  cutie_mark = "three apples"
))

# The default behavior is to generate a SELECT query
rows_insert(ponies, applejack, conflict = "ignore")
#> Matching, by = "name"
#> # A query:  ?? x 3
#> # Database: sqlite 3.51.1 [:memory:]
#>   id    name       cutie_mark  
#>   <lgl> <chr>      <chr>       
#> 1 NA    Apple Jack three apples
# And the original table is left unchanged:
ponies
#> # A query:  ?? x 3
#> # Database: sqlite 3.51.1 [:memory:]
#> # ℹ 3 variables: id <int>, name <chr>, cutie_mark <chr>

# You can also choose to modify the table with in_place = TRUE:
rows_insert(ponies, applejack, conflict = "ignore", in_place = TRUE)
#> Matching, by = "name"
# In this case `rows_insert()` returns nothing and the underlying
# data is modified
ponies
#> # A query:  ?? x 3
#> # Database: sqlite 3.51.1 [:memory:]
#>      id name       cutie_mark  
#>   <int> <chr>      <chr>       
#> 1     1 Apple Jack three apples
```
