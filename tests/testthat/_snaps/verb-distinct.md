# distinct() produces optimized SQL

    Code
      (out <- distinct(head(lf, 2), x, y))
    Output
      <SQL>
      SELECT DISTINCT "q01".*
      FROM (
        SELECT "df".*
        FROM "df"
        LIMIT 2
      ) AS "q01"

# distinct() after join is inlined

    Code
      show_query(out)
    Output
      <SQL>
      SELECT DISTINCT "df_LHS".*, "z"
      FROM "df" AS "df_LHS"
      LEFT JOIN "df" AS "df_RHS"
        ON ("df_LHS"."x" = "df_RHS"."x")

# distinct ignores groups when computing variables (#1081)

    Code
      show_query(db_distinct)
    Output
      <SQL>
      SELECT DISTINCT `g`, COUNT(*) OVER () AS `n`
      FROM `df`

# distinct respects window_order when .keep_all is TRUE

    Code
      distinct(window_order(lf, desc(y)), x, .keep_all = TRUE)
    Output
      <SQL>
      SELECT "x", "y"
      FROM (
        SELECT
          "df".*,
          ROW_NUMBER() OVER (PARTITION BY "x" ORDER BY "y" DESC) AS "col01"
        FROM "df"
      ) AS "q01"
      WHERE ("col01" = 1)

# distinct uses dummy window order when .keep_all is TRUE and no order is used

    Code
      distinct(lf, x, .keep_all = TRUE)
    Output
      <SQL>
      SELECT "x", "y"
      FROM (
        SELECT "df".*, ROW_NUMBER() OVER (PARTITION BY "x" ORDER BY "x") AS "col01"
        FROM "df"
      ) AS "q01"
      WHERE ("col01" = 1)

