# check_sql() checks inputs

    Code
      f("x")
    Condition
      Error in `f()`:
      ! `x` must be a <sql>, not the string "x".

# check_sql() optionally checks names

    Code
      f(sql(a = "x"))
    Condition
      Error in `f()`:
      ! `x` must be an unnamed <sql>, not a <sql> object.

# check_sql() optionally allows NULL

    Code
      f("x")
    Condition
      Error in `f()`:
      ! `x` must be a <sql> or `NULL`, not the string "x".

