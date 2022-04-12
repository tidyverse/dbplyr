# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE `schema`.`tbl`

---

    Code
      sql_query_explain(con, sql("SELECT * FROM table"))
    Output
      <SQL> EXPLAIN SELECT * FROM table

---

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
        ON (`LHS`.`x` <=> `RHS`.`x`)

---

    Code
      full_join(lf, lf, by = "x")
    Condition
      Error in `sql_query_join()`:
      ! MySQL does not support full joins

---

    Code
      slice_sample(lf, 5)
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT `x`, ROW_NUMBER() OVER (ORDER BY RAND()) AS `q01`
        FROM `df`
      ) `q01`
      WHERE (`q01` <= 1)

---

    Code
      sql_values(con, tibble(x = 1:2, y = letters[1:2]))
    Output
      <SQL> SELECT CAST(`x` AS INTEGER) AS `x`, CAST(`y` AS CHAR) AS `y`
      FROM (
        (
          SELECT NULL AS `x`, NULL AS `y`
          WHERE (0 = 1)
        )
        UNION ALL
        (VALUES ROW(1, 'a'), ROW(2, 'b'))
      ) `values_table`

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

