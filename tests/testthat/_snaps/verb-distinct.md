# distinct throws error if column is specified and .keep_all is TRUE

    Code
      mf %>% distinct(x, .keep_all = TRUE) %>% collect()
    Condition
      Error:
      ! Can only find distinct value of specified columns if .keep_all is FALSE

