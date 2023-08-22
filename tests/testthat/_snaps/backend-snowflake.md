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

# row_number() with and without group_by() and arrange(): unordered defaults to Ordering by NULL (per empty_order)

    Code
      mf %>% mutate(rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS `rown`
      FROM `df`

---

    Code
      mf %>% group_by(y) %>% mutate(rown = row_number())
    Output
      <SQL>
      SELECT
        `df`.*,
        ROW_NUMBER() OVER (PARTITION BY `y` ORDER BY (SELECT NULL)) AS `rown`
      FROM `df`

---

    Code
      mf %>% arrange(y) %>% mutate(rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER (ORDER BY `y`) AS `rown`
      FROM `df`
      ORDER BY `y`

