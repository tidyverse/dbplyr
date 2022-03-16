# cut checks arguments

    Code
      (expect_error(translate_sql(cut(x, 1))))
    Output
      <error/rlang_error>
      Error in `cut()`:
      ! `breaks` must have size 2 or bigger.
    Code
      (expect_error(translate_sql(cut(x, c(1, 1)))))
    Output
      <error/rlang_error>
      Error in `cut()`:
      ! `breaks` are not unique.

