# argument src is deprecated

    Code
      tbl_lazy(mtcars, src = simulate_sqlite())
    Condition
      Error in `tbl_lazy()`:
      ! `...` must be empty.
      x Problematic argument:
      * src = simulate_sqlite()

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

# names() inform that they aren't meant to be used

    Code
      names(lazy_frame(x = 1))
    Message
      Did you mean `colnames()`?
    Output
      [1] "lazy_query" "src"       

