# Lazy operations

This set of S3 classes describe the action of dplyr verbs. These are
currently used for SQL sources to separate the description of operations
in R from their computation in SQL. This API is very new so is likely to
evolve in the future.

## Usage

``` r
lazy_base_query(x, vars, class = character(), ...)

op_grps(op)

op_vars(op)

op_sort(op)

op_frame(op)
```

## Details

`op_vars()` and `op_grps()` compute the variables and groups from a
sequence of lazy operations. `op_sort()` and `op_frame()` tracks the
order and frame for use in window functions.
