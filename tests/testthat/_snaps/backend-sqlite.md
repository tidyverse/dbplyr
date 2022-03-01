# custom SQL translation

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
        ON (`LHS`.`x` IS `RHS`.`x`)

# case_when translates correctly to ELSE when TRUE ~ is used

    Code
      translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", TRUE ~ "undefined"),
      con = simulate_sqlite())
    Output
      <SQL> CASE WHEN (`x` = 1) THEN 'yes' WHEN (`x` = 0) THEN 'no' ELSE 'undefined' END

# full and right join

    Code
      full_join(df1, df2, by = "x")
    Output
      <SQL>
      SELECT `x`, `y.x`, `y.y`, `z`
      FROM (
        SELECT
          COALESCE(`LHS`.`x`, `RHS`.`x`) AS `x`,
          `LHS`.`y` AS `y.x`,
          `RHS`.`y` AS `y.y`,
          `z`
        FROM `df` AS `LHS`
        LEFT JOIN `df` AS `RHS`
          ON (`LHS`.`x` = `RHS`.`x`)
        UNION
        SELECT
          COALESCE(`RHS`.`x`, `LHS`.`x`) AS `x`,
          `LHS`.`y` AS `y.x`,
          `RHS`.`y` AS `y.y`,
          `z`
        FROM `df` AS `RHS`
        LEFT JOIN `df` AS `LHS`
          ON (`RHS`.`x` = `LHS`.`x`)
      ) AS `q01`

---

    Code
      right_join(df2, df1, by = "x")
    Output
      <SQL>
      SELECT `RHS`.`x` AS `x`, `LHS`.`y` AS `y.x`, `z`, `RHS`.`y` AS `y.y`
      FROM `df` AS `RHS`
      LEFT JOIN `df` AS `LHS`
        ON (`RHS`.`x` = `LHS`.`x`)

# can explain a query

    Code
      db %>% filter(x > 2) %>% explain()
    Output
      <SQL>
      SELECT *
      FROM `test`
      WHERE (`x` > 2.0)
      
      <PLAN>
        id parent notused                                        detail
      1  2      0       0 SEARCH test USING COVERING INDEX test_x (x>?)

