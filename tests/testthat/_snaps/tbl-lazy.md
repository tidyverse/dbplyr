# cannot convert tbl_lazy to data.frame

    Code
      as.data.frame(tbl_lazy(mtcars, con = simulate_sqlite()))
    Condition
      Error in `as.data.frame()`:
      ! Can not coerce <tbl_lazy> to <data.frame>

# has print method

    Code
      tbl_lazy(mtcars)
    Output
      <SQL>
      SELECT *
      FROM `df`

