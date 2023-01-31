# custom scalar translated correctly

    Code
      (expect_error(translate_sql(grepl("exp", x, ignore.case = TRUE))))
    Output
      <error/rlang_error>
      Error in `grepl()`:
      ! `ignore.case = TRUE` isn't supported in Snowflake translation.
      i It must be FALSE instead.

