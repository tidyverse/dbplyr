# shiny objects give useful errors

    Code
      lf %>% filter(a == input$x) %>% show_query()
    Condition
      Error:
      ! Cannot translate shiny inputs to SQL.
      * Force evaluation in R with (e.g.) `!!input$x` or `local(input$x)`

---

    Code
      lf %>% filter(a == x()) %>% show_query()
    Condition
      Error:
      ! Cannot translate a shiny reactive to SQL.
      * Force evaluation in R with (e.g.) `!!foo()` or `local(foo())`

