# window_order errors for data frame

    Code
      (expect_error(window_order(data.frame(x = 1))))
    Output
      <error/rlang_error>
      Error in `window_order()`:
      ! `.data` must be a <tbl_lazy>, not a <data.frame>.
      i Did you mean to use `arrange()` instead?
    Code
      (expect_error(window_order("a")))
    Output
      <error/rlang_error>
      Error in `window_order()`:
      ! `.data` must be a <tbl_lazy>, not a <character>.

# window_frame errors for data frame

    Code
      (expect_error(window_frame(data.frame(x = 1))))
    Output
      <error/rlang_error>
      Error in `window_frame()`:
      ! `.data` must be a <tbl_lazy>, not a <data.frame>.

