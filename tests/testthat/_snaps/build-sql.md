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

# gives informative errors

    Code
      sql_glue2(con, "{y*}")
    Condition
      Error in `sql_glue2()`:
      ! Failed to interpolate {y*}.
      Caused by error:
      ! object 'y' not found
    Code
      sql_glue2(con, "{1 + }")
    Condition
      Error in `sql_glue2()`:
      ! Failed to interpolate {1 + }.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: 1 + 
         ^
    Code
      sql_glue2(con, "{.bar x}")
    Condition
      Error in `sql_glue2()`:
      ! Unknown marker "bar" in {.bar x}.
