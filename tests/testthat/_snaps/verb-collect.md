# collect() handles DBI error

    Code
      (expect_error(collect(mutate(mf, a = sql("invalid sql")))))
    Output
      <error/rlang_error>
      Error in `collect()`:
      ! Failed to collect lazy table.
      Caused by error:
      ! dummy DBI error

