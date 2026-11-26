# -Inf and Inf error by default

    Code
      escape(Inf, con = con)
    Condition
      Error in `sql_escape_infinity()`:
      ! Don't know how to escape infinity for this connection.
      i Please file an issue: <https://github.com/tidyverse/dbplyr/issues>

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

# other objects get informative error

    Code
      filter(lf, x == input)
    Condition
      Error:
      ! Cannot translate shiny inputs to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!input$x` or `local(input$x)`?
    Code
      filter(lf, x == x())
    Condition
      Error in `filter()`:
      i In argument: `x == x()`
      Caused by error:
      ! Cannot translate a shiny reactive to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!foo()` or `local(foo())`?
    Code
      filter(lf, x == df)
    Condition
      Error:
      ! Cannot translate a data.frame to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!df$x` or `local(df$x)`?
    Code
      filter(lf, x == mean)
    Condition
      Error:
      ! Cannot translate a function to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!x` or `local(x)`?

