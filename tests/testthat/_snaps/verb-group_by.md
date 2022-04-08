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
      Error in `group_by.tbl_lazy()`:
      ! Must group by variables found in `.data`.
      x Column `y` is not found.

