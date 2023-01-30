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
      <error/rlang_error>
      Error in `group_by()`:
      i In argument: `y`
      Caused by error:
      ! Object `y` not found.

# group_by() produces nice error messages

    Code
      lf %>% group_by(z = non_existent + 1)
    Condition
      Error in `group_by()`:
      i In argument: `z = non_existent + 1`
      Caused by error:
      ! Object `non_existent` not found.
    Code
      lf %>% group_by(across(non_existent))
    Condition
      Error in `group_by()`:
      i In argument: `across(non_existent)`
      Caused by error in `across()`:
      ! Can't subset columns that don't exist.
      x Column `non_existent` doesn't exist.

# ungroup() produces nice error messages

    Code
      lazy_frame(x = 1) %>% ungroup(non_existent)
    Condition
      Error in `ungroup()`:
      ! Can't subset columns that don't exist.
      x Column `non_existent` doesn't exist.

