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
      slice_sample(lf, 5)
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT `x`, ROW_NUMBER() OVER (ORDER BY RAND()) AS `q01`
        FROM `df`
      ) `q01`
      WHERE (`q01` <= 1)

# `sql_query_rows_update()` is correct

    Code
      sql_query_rows_update(con = simulate_mysql(), x_name = ident("df_x"), y = df_y,
      by = c("a", "b"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> UPDATE `df_x`
      INNER JOIN (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        ON `...y`.`a` = `df_x`.`a` AND `...y`.`b` = `df_x`.`b`
      SET `df_x`.`c` = `...y`.`c`, `df_x`.`d` = `...y`.`d`
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

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

