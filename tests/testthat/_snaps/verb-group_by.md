# errors for .drop = FALSE

    Code
      lazy_frame(x = 1:3, y = 1:3) %>% group_by(y, .drop = FALSE)
    Condition
      Error in `group_by()`:
      ! `.drop` is not supported with database backends

