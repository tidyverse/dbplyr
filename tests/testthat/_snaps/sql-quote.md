# sql_quote() checks its inputs

    Code
      sql_quote(1)
    Condition
      Error in `sql_quote()`:
      ! `x` must be a character vector, not the number 1.
    Code
      sql_quote("x", 1)
    Condition
      Error in `sql_quote()`:
      ! `quote` must be a character vector, not the number 1.
    Code
      sql_quote("x", character())
    Condition
      Error in `sql_quote()`:
      ! `quote` must be length 1 or 2.

