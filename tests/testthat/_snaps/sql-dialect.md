# new_sql_dialect() validates inputs

    Code
      new_sql_dialect(1)
    Condition
      Error in `new_sql_dialect()`:
      ! `dialect` must be a single string, not the number 1.
    Code
      new_sql_dialect("x", quote_identifier = "a function")
    Condition
      Error in `new_sql_dialect()`:
      ! `quote_identifier` must be a function, not the string "a function".
    Code
      new_sql_dialect("x", quote_identifier = function(x) x, has_window_clause = "yes")
    Condition
      Error in `new_sql_dialect()`:
      ! `has_window_clause` must be `TRUE` or `FALSE`, not the string "yes".

# print method shows class

    Code
      d
    Output
      <sql_dialect_test>

