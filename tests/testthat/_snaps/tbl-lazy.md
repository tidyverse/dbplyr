# argument src is deprecated

    Code
      dummy <- tbl_lazy(mtcars, src = simulate_sqlite())
    Condition
      Warning:
      The `src` argument of `tbl_lazy()` is deprecated as of dbplyr 1.4.0.
      i Please use the `con` argument instead.

# cannot convert tbl_lazy to data.frame

    Code
      as.data.frame(tbl_lazy(mtcars, src = simulate_sqlite()))
    Condition
      Warning:
      The `src` argument of `tbl_lazy()` is deprecated as of dbplyr 1.4.0.
      i Please use the `con` argument instead.
      Error in `as.data.frame()`:
      ! Can not coerce <tbl_lazy> to <data.frame>

# has print method

    Code
      tbl_lazy(mtcars)
    Output
      <SQL>
      SELECT *
      FROM `df`

