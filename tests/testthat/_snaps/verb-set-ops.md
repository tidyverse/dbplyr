# can combine multiple union in one query

    Code
      lf1 %>% union_all(lf2) %>% union(lf3)
    Output
      <SQL>
      SELECT `lf1`.*, NULL AS `z`
      FROM `lf1`
      
      UNION ALL
      
      SELECT `q01`.*, NULL AS `z`
      FROM (
        SELECT NULL AS `x`, `lf2`.*
        FROM `lf2`
      ) `q01`
      
      UNION
      
      SELECT NULL AS `x`, NULL AS `y`, `lf3`.*
      FROM `lf3`

