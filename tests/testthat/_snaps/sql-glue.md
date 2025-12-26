# useful error if bad identifier

    Code
      sql_glue2(con, "{.id 1}")
    Condition
      Error in `sql_glue2()`:
      ! {.id 1} must be passed a character vector.
    Code
      sql_glue2(con, "{.tbl 1}")
    Condition
      Error in `sql_glue2()`:
      ! Failed to interpolate {.tbl 1}.
      Caused by error in `glue_transformer()`:
      ! `x` must be a table source (SQL or a table identifier), not the number 1.
    Code
      sql_glue2(con, "{.sql 1}")
    Condition
      Error in `sql_glue2()`:
      ! {.sql 1} must be passed a string.

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

