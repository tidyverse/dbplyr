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
intersect(x, y, copy = FALSE, ..., all = FALSE)

# S3 method for class 'tbl_lazy'
union(x, y, copy = FALSE, ..., all = FALSE)

# S3 method for class 'tbl_lazy'
union_all(x, y, copy = FALSE, ...)

# S3 method for class 'tbl_lazy'
setdiff(x, y, copy = FALSE, ..., all = FALSE)
```

## Arguments

- x, y:

  A pair of lazy data frames backed by database queries.

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into a temporary table in same
  database as `x`. `*_join()` will automatically run `ANALYZE` on the
  created table in the hope that this will make you queries as efficient
  as possible by giving more data to the query planner.

  This allows you to join tables across srcs, but it's potentially
  expensive operation so you must opt into it.

- ...:

  Must be empty.

- all:

  If `TRUE`, includes all matches in output, not just unique rows.
