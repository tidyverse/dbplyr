# print method doesn't change unexpectedly

    Code
      sql_build(semi_join(lf1, filter(lf2, z == 2)))
    Message
      Joining with `by = join_by(x)`
    Output
      <SQL SEMI JOIN>
      By:
        x-x
      Where:
        "df_RHS"."z" = 2.0
      X:
        <table_path> "df"
      Y:
        <table_path> "df"

# generated sql doesn't change unexpectedly

    Code
      semi_join(lf, lf)
    Message
      Joining with `by = join_by(x, y)`
    Output
      <SQL>
      SELECT "df_LHS".*
      FROM "df" AS "df_LHS"
      WHERE EXISTS (
        SELECT 1 FROM "df" AS "df_RHS"
        WHERE ("df_LHS"."x" = "df_RHS"."x") AND ("df_LHS"."y" = "df_RHS"."y")
      )

---

    Code
      anti_join(lf, lf)
    Message
      Joining with `by = join_by(x, y)`
    Output
      <SQL>
      SELECT "df_LHS".*
      FROM "df" AS "df_LHS"
      WHERE NOT EXISTS (
        SELECT 1 FROM "df" AS "df_RHS"
        WHERE ("df_LHS"."x" = "df_RHS"."x") AND ("df_LHS"."y" = "df_RHS"."y")
      )

