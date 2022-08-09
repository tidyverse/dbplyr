# collect() handles DBI error

    Code
      (expect_error(mf %>% mutate(a = sql("invalid sql")) %>% collect()))
    Output
      <error/rlang_error>
      Error in `collect()`:
      ! Failed to collect lazy table.
      Caused by error:
      ! near "AS": syntax error

