# .preserve is not supported

    Code
      lf %>% filter(x == 1, .preserve = TRUE)
    Condition
      Error in `filter()`:
      ! `.preserve` is not supported on database backends

