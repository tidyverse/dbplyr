# errors for .drop = FALSE

    Code
      lazy_frame(x = 1:3, y = 1:3) %>% group_by(y, .drop = FALSE)
    Condition
      Error in `group_by()`:
      ! `.drop` is not supported with database backends

# informative errors for missing variables

    Code
      (expect_error(lazy_frame(x = 1:3) %>% group_by(y)))
    Output
      <error/dbplyr_symbol_not_found>
      Error in `partial_eval_sym()`:
      ! object `y` not found.

# group_by() produces nice error messages

    Code
      lf %>% group_by(across(non_existent))
    Condition
      Error in `lapply()`:
      ! Can't subset columns that don't exist.
      x Column `non_existent` doesn't exist.

# ungroup() produces nice error messages

    Code
      lazy_frame(x = 1) %>% ungroup(non_existent)
    Condition
      Error in `ungroup()`:
      ! Can't subset columns that don't exist.
      x Column `non_existent` doesn't exist.

