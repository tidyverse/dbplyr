# print method renders sql

    Code
      lf$lazy_query
    Output
      -- <lazy_select_query> -------------------------
      SELECT "df".*, "x" + 1.0 AS "y"
      FROM "df"
    Code
      sql_build(lf)
    Output
      -- <select_query> -------------------------
      SELECT "df".*, "x" + 1.0 AS "y"
      FROM "df"

