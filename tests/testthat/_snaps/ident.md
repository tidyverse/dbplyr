# can format ident

    Code
      ident()
    Output
      <IDENT> [empty]

# as_ident_or_sql() gives informative error for non-character

    Code
      as_ident_or_sql(1)
    Condition
      Error:
      ! Invalid identifier: expecting a character vector.

