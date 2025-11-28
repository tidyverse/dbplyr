# errors for .drop = FALSE

    Code
      group_by(lazy_frame(x = 1:3, y = 1:3), y, .drop = FALSE)
    Condition
      Error in `group_by()`:
      ! `.drop = FALSE` isn't supported on database backends.
      i It must be TRUE instead.

# informative errors for missing variables

    Code
      (expect_error(group_by(lazy_frame(x = 1:3), y)))
    Output
      <error/rlang_error>
      Error in `group_by()`:
      i In argument: `y`
      Caused by error:
      ! Object `y` not found.

# group_by() produces nice error messages

    Code
      group_by(lf, z = non_existent + 1)
    Condition
      Error in `group_by()`:
      i In argument: `z = non_existent + 1`
      Caused by error:
      ! Object `non_existent` not found.
    Code
      group_by(lf, across(non_existent))
    Condition
      Error in `group_by()`:
      i In argument: `across(non_existent)`
      Caused by error in `across()`:
      ! Can't select columns that don't exist.
      x Column `non_existent` doesn't exist.

# ungroup() produces nice error messages

    Code
      ungroup(lazy_frame(x = 1), non_existent)
    Condition
      Error in `ungroup()`:
      ! Can't select columns that don't exist.
      x Column `non_existent` doesn't exist.

