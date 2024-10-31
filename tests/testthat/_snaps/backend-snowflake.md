# pasting translated correctly

    Code
      test_translate_sql(paste0(x, collapse = ""))
    Condition
      Error in `check_collapse()`:
      ! `collapse` not supported in DB translation of `paste()`.
      i Please use `str_flatten()` instead.

# difftime is translated correctly

    Code
      test_translate_sql(difftime(start_date, end_date, units = "auto"))
    Condition
      Error in `difftime()`:
      ! The only supported value for `units` on SQL backends is "days"

---

    Code
      test_translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days"))
    Condition
      Error in `difftime()`:
      ! The `tz` argument is not supported for SQL backends.

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

