# build_sql() requires connection

    Code
      build_sql("SELECT * FROM ", x)
    Condition
      Error in `build_sql()`:
      ! `con` must not be NULL.

# glue_sql() checks size

    Code
      glue_sql2("{.col x}", .con = con)
    Condition
      Error in `.transformer()`:
      ! `value` must have size 1, not 2.
    Code
      glue_sql2("{.col character()}", .con = con)
    Condition
      Error in `.transformer()`:
      ! `value` must have size 1, not 0.

# glue_sql() can collapse

    Code
      glue_sql2("{.tbl x*}", .con = con)
    Condition
      Error in `glue_check_collapse()`:
      ! Collapsing is only allowed for "col" and "val", not for "tbl".
    Code
      glue_sql2("{.name x*}", .con = con)
    Condition
      Error in `glue_check_collapse()`:
      ! Collapsing is only allowed for "col" and "val", not for "name".
    Code
      glue_sql2("{.sql x*}", .con = con)
    Condition
      Error in `glue_check_collapse()`:
      ! Collapsing is only allowed for "col" and "val", not for "sql".
    Code
      glue_sql2("{.from x*}", .con = con)
    Condition
      Error in `glue_check_collapse()`:
      ! Collapsing is only allowed for "col" and "val", not for "from".

