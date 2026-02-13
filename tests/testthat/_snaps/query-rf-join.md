# generated sql doesn't change unexpectedly

    Code
      right_join(lf, lf)
    Message
      Joining with `by = join_by(x, y)`
    Output
      <SQL>
      SELECT "df_RHS".*
      FROM "df" AS "df_LHS"
      RIGHT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x" AND "df_LHS"."y" = "df_RHS"."y")

---

    Code
      full_join(lf, lf)
    Message
      Joining with `by = join_by(x, y)`
    Output
      <SQL>
      SELECT
        COALESCE("df_LHS"."x", "df_RHS"."x") AS "x",
        COALESCE("df_LHS"."y", "df_RHS"."y") AS "y"
      FROM "df" AS "df_LHS"
      FULL JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x" AND "df_LHS"."y" = "df_RHS"."y")

# disambiguate variables that only differ in case

    Code
      full_join(lf1, lf2, by = "y")
    Output
      <SQL>
      SELECT
        "df_LHS"."x" AS "x",
        COALESCE("df_LHS"."y", "df_RHS"."y") AS "y",
        "df_RHS"."X" AS "X"
      FROM "df" AS "df_LHS"
      FULL JOIN "df" AS "df_RHS"
        ON ("df_LHS"."y" = "df_RHS"."y")

# sql_on query doesn't change unexpectedly

    Code
      right_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT "LHS"."x" AS "x.x", "y", "RHS"."x" AS "x.y", "z"
      FROM "df" AS "LHS"
      RIGHT JOIN "df" AS "RHS"
        ON (LHS.y < RHS.z)

---

    Code
      full_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT "LHS"."x" AS "x.x", "y", "RHS"."x" AS "x.y", "z"
      FROM "df" AS "LHS"
      FULL JOIN "df" AS "RHS"
        ON (LHS.y < RHS.z)

