# multi_join where clause uses qualified column names

    Code
      filter(left_join(lf1, lf2, by = "x"), y > 1)
    Output
      <SQL>
      SELECT "df_LHS".*, "z"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x")
      WHERE ("df_LHS"."y" > 1.0)

---

    Code
      filter(left_join(lf1, lf2, by = "x"), z > 1)
    Output
      <SQL>
      SELECT "df_LHS".*, "z"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x")
      WHERE ("df_RHS"."z" > 1.0)

---

    Code
      filter(left_join(lf1, lf2, by = "x"), y > 1, z < 5)
    Output
      <SQL>
      SELECT "df_LHS".*, "z"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x")
      WHERE ("df_LHS"."y" > 1.0) AND ("df_RHS"."z" < 5.0)

---

    Code
      filter(left_join(lf1, lf3, by = "x"), y.x > 1)
    Output
      <SQL>
      SELECT "df_LHS"."x" AS "x", "df_LHS"."y" AS "y.x", "df_RHS"."y" AS "y.y"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x")
      WHERE ("df_LHS"."y" > 1.0)

# generated sql doesn't change unexpectedly

    Code
      inner_join(lf, lf)
    Message
      Joining with `by = join_by(x, y)`
    Output
      <SQL>
      SELECT "df_LHS".*
      FROM "df" AS "df_LHS"
      INNER JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x" AND "df_LHS"."y" = "df_RHS"."y")

---

    Code
      left_join(lf, lf)
    Message
      Joining with `by = join_by(x, y)`
    Output
      <SQL>
      SELECT "df_LHS".*
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x" AND "df_LHS"."y" = "df_RHS"."y")

# only disambiguates shared variables

    Code
      left_join(lf1, lf2, by = c(x = "x"))
    Output
      <SQL>
      SELECT "df_LHS".*, "z"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x")

---

    Code
      left_join(lf1, lf2, by = c(y = "z"))
    Output
      <SQL>
      SELECT "df_LHS"."x" AS "x.x", "y", "df_RHS"."x" AS "x.y"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."y" = "df_RHS"."z")

# disambiguate variables that only differ in case

    Code
      left_join(lf1, lf2, by = "y")
    Output
      <SQL>
      SELECT "df_LHS".*, "df_RHS"."X" AS "X"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."y" = "df_RHS"."y")

# sql_on query doesn't change unexpectedly

    Code
      inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT "LHS"."x" AS "x.x", "y", "RHS"."x" AS "x.y", "z"
      FROM "df" AS "LHS"
      INNER JOIN "df" AS "RHS"
        ON (LHS.y < RHS.z)

---

    Code
      left_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT "LHS"."x" AS "x.x", "y", "RHS"."x" AS "x.y", "z"
      FROM "df" AS "LHS"
      LEFT JOIN "df" AS "RHS"
        ON (LHS.y < RHS.z)

