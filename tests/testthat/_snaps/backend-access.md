# custom scalar translated correctly

    Code
      translate_sql(paste(x, collapse = "-"), con = con)
    Condition
      Error in `check_collapse()`:
      ! `collapse` not supported in DB translation of `paste()`.
      i Please use `str_flatten()` instead.

# queries translate correctly

    Code
      head(mf)
    Output
      <SQL>
      SELECT TOP 6 *
      FROM "df"

# multiple joins use parens #1576

    Code
      inner_join(left_join(lf1, lf2, by = "x"), lf3, by = "x")
    Output
      <SQL>
      SELECT "lf1".*, "b", "c"
      FROM (("lf1"
      LEFT JOIN "lf2"
      ON "lf1"."x" = "lf2"."x")
      INNER JOIN "lf3"
      ON "lf1"."x" = "lf3"."x")

---

    Code
      left_join(inner_join(left_join(lf1, lf2, by = "x"), lf3, by = "x"), lf4, by = "x")
    Output
      <SQL>
      SELECT "lf1".*, "b", "c", "d"
      FROM ((("lf1"
      LEFT JOIN "lf2"
      ON "lf1"."x" = "lf2"."x")
      INNER JOIN "lf3"
      ON "lf1"."x" = "lf3"."x")
      LEFT JOIN "lf4"
      ON "lf1"."x" = "lf4"."x")

