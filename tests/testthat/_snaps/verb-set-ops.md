# can combine multiple union in one query

    Code
      union(union_all(lf1, lf2), lf3)
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
      show_query(left_join(union(union_all(lf1, lf2), lf3), lf1, by = "x"),
      sql_options = sql_options(cte = TRUE))
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
      ),
      `q05` AS (
        SELECT *
        FROM `q01`
      
        UNION ALL
      
        SELECT *
        FROM `q03`
      
        UNION
      
        SELECT *
        FROM `q04`
      )
      SELECT `LHS`.`x` AS `x`, `LHS`.`y` AS `y.x`, `z`, `lf1`.`y` AS `y.y`
      FROM `q05` AS `LHS`
      LEFT JOIN `lf1`
        ON (`LHS`.`x` = `lf1`.`x`)

