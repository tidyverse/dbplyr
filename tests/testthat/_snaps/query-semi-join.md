# lazy_semi_join_query() checks arguments

    Code
      (my_lazy_semi_join_query(x = lazy_frame(x = 1)))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `x` must be a lazy query, not a <tbl_TestConnection> object.
    Code
      (my_lazy_semi_join_query(y = lazy_frame(x = 1)))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `y` must be a lazy query, not a <tbl_TestConnection> object.

---

    Code
      (my_lazy_semi_join_query(by = lmod(by0, x = 1)))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `by$x` must be a character vector, not the number 1.

---

    Code
      (my_lazy_semi_join_query(anti = NA))
    Condition
      Error in `my_lazy_semi_join_query()`:
      ! `anti` must be `TRUE` or `FALSE`, not `NA`.

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

