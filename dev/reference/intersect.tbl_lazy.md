# SQL set operations

These are methods for the dplyr generics
[`dplyr::intersect()`](https://generics.r-lib.org/reference/setops.html),
[`dplyr::union()`](https://generics.r-lib.org/reference/setops.html),
and
[`dplyr::setdiff()`](https://generics.r-lib.org/reference/setops.html).
They are translated to `INTERSECT`, `UNION`, and `EXCEPT` respectively.

## Usage

``` r
# S3 method for class 'tbl_lazy'
intersect(x, y, copy = "none", ..., all = FALSE)

# S3 method for class 'tbl_lazy'
union(x, y, copy = "none", ..., all = FALSE)

# S3 method for class 'tbl_lazy'
union_all(x, y, copy = "none", ...)

# S3 method for class 'tbl_lazy'
setdiff(x, y, copy = "none", ..., all = FALSE)
```

## Arguments

- x, y:

  A pair of lazy data frames backed by database queries.

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

- ...:

  Must be empty.

- all:

  If `TRUE`, includes all matches in output, not just unique rows.
