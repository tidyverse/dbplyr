# custom scalar translated correctly

    Code
      (expect_error(test_translate_sql(grepl("exp", x, ignore.case = TRUE))))
    Output
      <error/rlang_error>
      Error in `grepl()`:
      ! `ignore.case = TRUE` isn't supported in Snowflake translation.
      i It must be FALSE instead.

# pmin() and pmax() respect na.rm

    Code
      test_translate_sql(pmin(x, y, z, na.rm = TRUE))
    Output
      <SQL> COALESCE(IFF(COALESCE(IFF(`x` <= `y`, `x`, `y`), `x`, `y`) <= `z`, COALESCE(IFF(`x` <= `y`, `x`, `y`), `x`, `y`), `z`), COALESCE(IFF(`x` <= `y`, `x`, `y`), `x`, `y`), `z`)

---

    Code
      test_translate_sql(pmax(x, y, z, na.rm = TRUE))
    Output
      <SQL> COALESCE(IFF(COALESCE(IFF(`x` >= `y`, `x`, `y`), `x`, `y`) >= `z`, COALESCE(IFF(`x` >= `y`, `x`, `y`), `x`, `y`), `z`), COALESCE(IFF(`x` >= `y`, `x`, `y`), `x`, `y`), `z`)

