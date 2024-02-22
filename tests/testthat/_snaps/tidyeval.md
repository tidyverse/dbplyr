# other objects get informative error

    Code
      capture_dot(lf, input)
    Condition
      Error:
      ! Cannot translate shiny inputs to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!input$x` or `local(input$x)`?
    Code
      capture_dot(lf, x())
    Output
      x()
    Code
      capture_dot(lf, df)
    Condition
      Error:
      ! Cannot translate a data.frame to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!df$x` or `local(df$x)`?

