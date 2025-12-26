# build_sql() is deprecated

    Code
      build_sql("SELECT * FROM TABLE", con = con)
    Condition
      Warning:
      `build_sql()` was deprecated in dbplyr 2.6.0.
      i Please use `sql_glue2()` instead.
    Output
      <SQL> SELECT * FROM TABLE

# build_sql() requires connection

    Code
      build_sql("SELECT * FROM ", x)
    Condition
      Error in `build_sql()`:
      ! `con` must not be NULL.

