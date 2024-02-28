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
      lf %>% filter(x == input)
    Condition
      Error:
      ! Cannot translate shiny inputs to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!input$x` or `local(input$x)`?
    Code
      lf %>% filter(x == x())
    Condition
      Error:
      ! Cannot translate a shiny reactive to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!foo()` or `local(foo())`?
    Code
      lf %>% filter(x == df)
    Condition
      Error:
      ! Cannot translate a data.frame to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!df$x` or `local(df$x)`?
    Code
      lf %>% filter(x == mean)
    Condition
      Error:
      ! Cannot translate a function to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!x` or `local(x)`?

