# print method doesn't change unexpectedly

    Code
      sql_build(left_join(left_join(lf1, lf2, by = "x"), lf3, by = "x"))
    Output
      <SQL JOINS>
      X:
        <table_path> "lf1"
      Type: left
      By:
        x-x
      Y:
        <table_path> "lf2"
      Type: left
      By:
        x-x
      Y:
        <table_path> "lf3"

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

---

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

# only disambiguates shared variables

    Code
      left_join(lf1, lf2)
    Message
      Joining with `by = join_by(x)`
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

---

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

---

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

---

    Code
      semi_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT "LHS".*
      FROM "df" AS "LHS"
      WHERE EXISTS (
        SELECT 1 FROM "df" AS "RHS"
        WHERE (LHS.y < RHS.z)
      )

---

    Code
      anti_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT "LHS".*
      FROM "df" AS "LHS"
      WHERE NOT EXISTS (
        SELECT 1 FROM "df" AS "RHS"
        WHERE (LHS.y < RHS.z)
      )
