# bind_queries combines multiple lazy tables

    Code
      bind_queries(lf1, lf2, lf3)
    Output
      <SQL>
      SELECT *
      FROM "lf1"
      
      UNION ALL
      
      SELECT *
      FROM "lf2"
      
      UNION ALL
      
      SELECT *
      FROM "lf3"

# bind_queries works with splice operator

    Code
      bind_queries(!!!queries)
    Output
      <SQL>
      SELECT *
      FROM "lf1"
      
      UNION ALL
      
      SELECT *
      FROM "lf2"

# bind_queries checks its inputs

    Code
      bind_queries()
    Condition
      Error in `bind_queries()`:
      ! `bind_queries()` requires at least one input.
    Code
      bind_queries(lf, 1)
    Condition
      Error in `bind_queries()`:
      ! `..2` must be a lazy query, not the number 1.

