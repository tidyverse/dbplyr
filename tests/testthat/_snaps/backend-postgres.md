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

