# works with labels a character vector

    Code
      (expect_error(test_translate_sql(cut(x, 1:3, labels = c("a", "b", "c")))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `cut()`:
      ! Can't recycle `labels` (size 3) to size 2.

# cut checks arguments

    Code
      (expect_error(test_translate_sql(cut(x, 1))))
    Output
      <error/rlang_error>
      Error in `cut()`:
      ! `breaks` must have at least two values.
    Code
      (expect_error(test_translate_sql(cut(x, c(1, 1)))))
    Output
      <error/rlang_error>
      Error in `cut()`:
      ! `breaks` are not unique.
    Code
      (expect_error(test_translate_sql(cut(x, c(1, 2, NA)))))
    Output
      <error/rlang_error>
      Error in `cut()`:
      ! `breaks` values must not be missing.

