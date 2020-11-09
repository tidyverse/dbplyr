# custom SQL translation

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (`LHS`.`x` IS `RHS`.`x`)
      

# can explain a query

    Code
      db %>% filter(x > 2) %>% explain()
    Output
      <SQL>
      SELECT *
      FROM `test`
      WHERE (`x` > 2.0)
      
      <PLAN>
        id parent notused                                              detail
      1  2      0       0 SEARCH TABLE test USING COVERING INDEX test_x (x>?)

