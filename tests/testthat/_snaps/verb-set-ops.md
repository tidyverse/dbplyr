# can combine multiple unions in one query

    Code
      show_query(lf_union)
    Output
      <SQL>
      SELECT `lf1`.*, NULL AS `z`
      FROM `lf1`
      
      UNION ALL
      
      SELECT `q01`.*, NULL AS `z`
      FROM (
        SELECT NULL AS `x`, `lf2`.*
        FROM `lf2`
      ) AS `q01`
      
      UNION
      
      SELECT NULL AS `x`, NULL AS `y`, `lf3`.*
      FROM `lf3`

---

    Code
      show_query(lf_union, sql_options = with_cte)
    Output
      <SQL>
      WITH `q01` AS (
        SELECT `lf1`.*, NULL AS `z`
        FROM `lf1`
      ),
      `q02` AS (
        SELECT NULL AS `x`, `lf2`.*
        FROM `lf2`
      ),
      `q03` AS (
        SELECT `q01`.*, NULL AS `z`
        FROM `q02` AS `q01`
      ),
      `q04` AS (
        SELECT NULL AS `x`, NULL AS `y`, `lf3`.*
        FROM `lf3`
      )
      SELECT *
      FROM `q01`
      
      UNION ALL
      
      SELECT *
      FROM `q03`
      
      UNION
      
      SELECT *
      FROM `q04`

# set ops correctly quote reused queries in CTEs

    Code
      show_query(union_all(lf, lf), sql_options = sql_options(cte = TRUE))
    Output
      <SQL>
      WITH `q01` AS (
        SELECT `lf`.*, `x` + 1.0 AS `y`
        FROM `lf`
      )
      SELECT *
      FROM `q01`
      
      UNION ALL
      
      SELECT *
      FROM `q01`

