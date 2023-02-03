# index fails if columns are missing

    Code
      (expect_error(compute(mf, indexes = list(c("y", "x", "z"), "a"))))
    Output
      <error/rlang_error>
      Error in `compute()`:
      ! All columns specified through `indexes` must exist in `x`.
      i The following columns are missing from `indexes`: y, z, and a.
    Code
      (expect_error(compute(mf, unique_indexes = list(c("y", "x", "z"), "a"))))
    Output
      <error/rlang_error>
      Error in `compute()`:
      ! All columns specified through `unique_indexes` must exist in `x`.
      i The following columns are missing from `unique_indexes`: y, z, and a.

# collect() handles DBI error

    Code
      (expect_error(mf %>% mutate(a = sql("invalid sql")) %>% collect()))
    Output
      <error/rlang_error>
      Error in `collect()`:
      ! Failed to collect lazy table.
      Caused by error:
      ! dummy DBI error

