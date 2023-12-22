# unless they're reactive objects, data.frames, or lists

    Code
      lf %>% filter(a == input$x)
    Condition
      Error in `filter()`:
      i In argument: `a == input$x`
      Caused by error:
      ! Cannot translate shiny inputs to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!input$x` or `local(input$x)`?
    Code
      lf %>% filter(a == x())
    Condition
      Error:
      ! Cannot translate a shiny reactive to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!foo()` or `local(foo())`?
    Code
      lf %>% filter(a == df$foo)
    Condition
      Error in `filter()`:
      i In argument: `a == df$foo`
      Caused by error:
      ! Cannot translate a data.frame to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!df$x` or `local(df$x)`?
    Code
      lf %>% filter(a == l$foo)
    Condition
      Error in `filter()`:
      i In argument: `a == l$foo`
      Caused by error:
      ! Cannot translate a list to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!l` or `local(l)`?

