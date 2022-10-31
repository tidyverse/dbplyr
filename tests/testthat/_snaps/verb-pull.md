# ungroup() produces nice error messages

    Code
      memdb_frame(x = 1) %>% pull(non_existent)
    Condition
      Error in `pull()`:
      Caused by error:
      ! object 'non_existent' not found
    Code
      memdb_frame(x = 1) %>% pull(1000)
    Condition
      Error in `pull()`:
      ! Can't extract columns past the end.
      i Location 1000 doesn't exist.
      i There is only 1 column.

