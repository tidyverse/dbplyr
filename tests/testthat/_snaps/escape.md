# con must not be NULL

    Code
      escape("a")
    Condition
      Error in `escape()`:
      ! `con` must not be NULL.

---

    Code
      sql_vector("a")
    Condition
      Error in `sql_vector()`:
      ! `con` must not be NULL.

# data frames give useful errors

    Code
      escape(mtcars, con = simulate_dbi())
    Condition
      Error:
      ! Cannot translate a data.frame to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!df$x` or `local(df$x)`?

