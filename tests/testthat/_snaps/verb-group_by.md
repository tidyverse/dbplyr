# errors about add argument 

    Code
      gf <- mf %>% group_by(x) %>% group_by(y, add = TRUE)
    Condition
      Error:
      ! The `add` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.add` argument instead.

# errors for .drop = FALSE

    Code
      lazy_frame(x = 1:3, y = 1:3) %>% group_by(y, .drop = FALSE)
    Condition
      Error in `group_by()`:
      ! `.drop = FALSE` isn't supported on database backends.
      i It must be TRUE instead.

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
      ! Can't select columns that don't exist.
      x Column `non_existent` doesn't exist.

# ungroup() produces nice error messages

    Code
      lazy_frame(x = 1) %>% ungroup(non_existent)
    Condition
      Error in `ungroup()`:
      ! Can't select columns that don't exist.
      x Column `non_existent` doesn't exist.

