# build_sql() requires connection

    Code
      build_sql("SELECT * FROM ", x)
    Condition
      Error in `build_sql()`:
      ! `con` must not be NULL.

# as.sql() is deprecated

    Code
      as.sql(ident("x"))
    Condition
      Warning:
      `as.sql()` was deprecated in dbplyr 2.6.0.
      i Please use `as_table_path()` instead.
    Output
      <IDENT> x

