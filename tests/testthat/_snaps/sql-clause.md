# sql_select_clauses generates expected SQL

    Code
      sql_format_clauses(clauses, 0, con)
    Output
      <SQL> SELECT a, b, c
      FROM "table"
      WHERE (x > 1) AND (y < 2)
      GROUP BY "a", "b"
      HAVING (COUNT(*) > 5)
      WINDOW "win1" AS (PARTITION BY "a")
      ORDER BY "a" DESC
      LIMIT 10

# sql_select_clauses can generate multiple lines

    Code
      sql_format_clauses(clauses, 0, con)
    Output
      <SQL> SELECT
        variable1,
        variable2,
        variable3,
        variable4,
        variable5,
        variable6,
        variable7,
        variable8
      FROM "table"

