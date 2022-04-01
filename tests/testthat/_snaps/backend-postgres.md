# custom SQL translation

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
        ON (`LHS`.`x` IS NOT DISTINCT FROM `RHS`.`x`)

# can explain

    Code
      db %>% mutate(y = x + 1) %>% explain()
    Output
      <SQL>
      SELECT "x", "x" + 1.0 AS "y"
      FROM "test"
      
      <PLAN>
                                                 QUERY PLAN
      1 Seq Scan on test  (cost=0.00..1.04 rows=3 width=36)

---

    Code
      db %>% mutate(y = x + 1) %>% explain(format = "json")
    Output
      <SQL>
      SELECT "x", "x" + 1.0 AS "y"
      FROM "test"
      
      <PLAN>
                                                                                                                                                                                                                                                                 QUERY PLAN
      1 [\n  {\n    "Plan": {\n      "Node Type": "Seq Scan",\n      "Parallel Aware": false,\n      "Relation Name": "test",\n      "Alias": "test",\n      "Startup Cost": 0.00,\n      "Total Cost": 1.04,\n      "Plan Rows": 3,\n      "Plan Width": 36\n    }\n  }\n]

