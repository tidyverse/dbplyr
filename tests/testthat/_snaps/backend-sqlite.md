# custom SQL translation

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `df_LHS`.`x` AS `x`
      FROM `df` AS `df_LHS`
      LEFT JOIN `df` AS `df_RHS`
        ON (`df_LHS`.`x` IS `df_RHS`.`x`)

---

    Code
      translate_sql(runif(n()), con = con)
    Output
      <SQL> (0.5 + RANDOM() / 18446744073709551616.0)

# case_when translates correctly to ELSE when TRUE ~ is used

    Code
      translate_sql(case_when(x == 1L ~ "yes", x == 0L ~ "no", TRUE ~ "undefined"),
      con = con)
    Output
      <SQL> CASE WHEN (`x` = 1) THEN 'yes' WHEN (`x` = 0) THEN 'no' ELSE 'undefined' END

# can explain a query

    Code
      explain(filter(db, x > 2))
    Output
      <SQL>
      SELECT `test`.*
      FROM `test`
      WHERE (`x` > 2.0)
      
      <PLAN>
        id parent notused                                        detail
      1  2      0      35 SEARCH test USING COVERING INDEX test_x (x>?)

