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

