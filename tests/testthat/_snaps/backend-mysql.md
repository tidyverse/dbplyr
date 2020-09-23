# generates custom sql

    Code
      sql_analyze(con, ident("table"))
    Output
      <SQL> ANALYZE TABLE `table`

---

    Code
      sql_explain(con, sql("SELECT * FROM table"))
    Output
      <SQL> EXPLAIN SELECT * FROM table

# can explain

    Code
      db %>% mutate(y = x + 1) %>% explain()
    Output
      <SQL>
      SELECT `x`, `x` + 1.0 AS `y`
      FROM `test`
      
      <PLAN>
        id select_type table type possible_keys  key key_len  ref rows Extra
      1  1      SIMPLE  test  ALL          <NA> <NA>    <NA> <NA>    3      

