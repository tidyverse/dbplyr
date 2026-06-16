# pasting translated correctly

    Code
      translate_sql(paste0(x, collapse = ""), con = con)
    Condition
      Error in `check_collapse()`:
      ! `collapse` not supported in DB translation of `paste()`.
      i Please use `str_flatten()` instead.

# row_number() with and without group_by() and arrange(): unordered defaults to Ordering by NULL (per empty_order)

    Code
      mutate(mf, rown = row_number())
    Output
      <SQL>
      SELECT *, ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS "rown"
      FROM "df"

---

    Code
      mutate(group_by(mf, y), rown = row_number())
    Output
      <SQL>
      SELECT *, ROW_NUMBER() OVER (PARTITION BY "y" ORDER BY (SELECT NULL)) AS "rown"
      FROM "df"

---

    Code
      mutate(arrange(mf, y), rown = row_number())
    Output
      <SQL>
      SELECT *, ROW_NUMBER() OVER (ORDER BY "y") AS "rown"
      FROM "df"
      ORDER BY "y"

# window functions use inline OVER, not named WINDOW clause

    Code
      mutate(group_by(mf, a), b = mean(b))
    Output
      <SQL>
      SELECT "a", AVG("b") OVER (PARTITION BY "a") AS "b"
      FROM "df"

