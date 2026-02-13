# argument src is deprecated

    Code
      tbl_lazy(mtcars, src = dialect_sqlite())
    Condition
      Error in `tbl_lazy()`:
      ! `...` must be empty.
      x Problematic argument:
      * src = dialect_sqlite()

# cannot convert tbl_lazy to data.frame

    Code
      as.data.frame(tbl_lazy(mtcars, con = dialect_sqlite()))
    Condition
      Error in `as.data.frame()`:
      ! Can not coerce <tbl_lazy> to <data.frame>

# has print method

    Code
      tbl_lazy(mtcars)
    Output
      <SQL>
      SELECT *
      FROM "df"

# names() inform that they aren't meant to be used

    Code
      names(lazy_frame(x = 1))
    Message
      ! The `names()` method of <tbl_lazy> is for internal use only.
      i Did you mean `colnames()`?
    Output
      [1] "con"        "src"        "lazy_query"

# $ aborts when not used with src or lazy_query

    The `$` method of <tbl_lazy> is for internal use only.
    i Use `dplyr::pull()` to get the values in a column.

