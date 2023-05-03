# can combine multiple union in one query

    Code
      lf1 %>% union_all(lf2) %>% union(lf3)
    Output
      <SQL>
      SELECT *, NULL AS `z`
      FROM `lf1`
      
      UNION ALL
      
      SELECT *, NULL AS `z`
      FROM (
        SELECT NULL AS `x`, *
        FROM `lf2`
      ) `q01`
      
      UNION
      
      SELECT NULL AS `x`, NULL AS `y`, *
      FROM `lf3`

