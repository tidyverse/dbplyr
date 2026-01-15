# basic set ops generate expected SQL

    Code
      union(db1, db2)
    Output
      <SQL>
      SELECT *
      FROM "db1"
      
      UNION
      
      SELECT *
      FROM "db2"
    Code
      union_all(db1, db2)
    Output
      <SQL>
      SELECT *
      FROM "db1"
      
      UNION ALL
      
      SELECT *
      FROM "db2"
    Code
      intersect(db1, db2)
    Output
      <SQL>
      (
        SELECT *
        FROM "db1"
      )
      INTERSECT
      (
        SELECT *
        FROM "db2"
      )
    Code
      setdiff(db1, db2)
    Output
      <SQL>
      (
        SELECT *
        FROM "db1"
      )
      EXCEPT
      (
        SELECT *
        FROM "db2"
      )

# can combine multiple unions in one query

    Code
      show_query(lf_union)
    Output
      <SQL>
      SELECT *, NULL AS "z"
      FROM "lf1"
      
      UNION ALL
      
      SELECT *, NULL AS "z"
      FROM (
        SELECT NULL AS "x", *
        FROM "lf2"
      ) AS "q01"
      
      UNION
      
      SELECT NULL AS "x", NULL AS "y", *
      FROM "lf3"

---

    Code
      show_query(lf_union, sql_options = with_cte)
    Output
      <SQL>
      WITH "q01" AS (
        SELECT *, NULL AS "z"
        FROM "lf1"
      ),
      "q02" AS (
        SELECT NULL AS "x", *
        FROM "lf2"
      ),
      "q03" AS (
        SELECT *, NULL AS "z"
        FROM "q02" AS "q01"
      ),
      "q04" AS (
        SELECT NULL AS "x", NULL AS "y", *
        FROM "lf3"
      )
      SELECT *
      FROM "q01"
      
      UNION ALL
      
      SELECT *
      FROM "q03"
      
      UNION
      
      SELECT *
      FROM "q04"

# set ops correctly quote reused queries in CTEs

    Code
      show_query(union_all(lf, lf), sql_options = sql_options(cte = TRUE))
    Output
      <SQL>
      WITH "q01" AS (
        SELECT *, "x" + 1.0 AS "y"
        FROM "lf"
      )
      SELECT *
      FROM "q01"
      
      UNION ALL
      
      SELECT *
      FROM "q01"

