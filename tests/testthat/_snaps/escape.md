# shiny objects give useful errors

    Code
      lf %>% filter(a == input$x) %>% show_query()
    Condition
      Error in `purrr::map_chr()`:
      i In index: 1.
      Caused by error:
      ! Cannot translate shiny inputs to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!input$x` or `local(input$x)`?

---

    Code
      lf %>% filter(a == x()) %>% show_query()
    Condition
      Error:
      ! Cannot translate a shiny reactive to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!foo()` or `local(foo())`?

# con must not be NULL

    Code
      escape("a")
    Condition
      Error in `escape()`:
      ! `con` must not be NULL

---

    Code
      sql_vector("a")
    Condition
      Error in `sql_vector()`:
      ! `con` must not be NULL

# data frames give useful errors

    Code
      escape(mtcars, con = simulate_dbi())
    Condition
      Error:
      ! Cannot translate a data.frame to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!df$x` or `local(df$x)`?

