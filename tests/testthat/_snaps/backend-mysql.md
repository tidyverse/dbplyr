# generates custom sql

    Code
      sql_table_analyze(con_maria, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE `schema`.`tbl`

---

    Code
      sql_query_explain(con_maria, sql("SELECT * FROM table"))
    Output
      <SQL> EXPLAIN SELECT * FROM table

---

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `df_LHS`.`x` AS `x`
      FROM `df` AS `df_LHS`
      LEFT JOIN `df` AS `df_RHS`
        ON (`df_LHS`.`x` <=> `df_RHS`.`x`)

---

    Code
      full_join(lf, lf, by = "x")
    Condition
      Error in `sql_query_join()`:
      ! MySQL does not support full joins

---

    Code
      slice_sample(lf, n = 1)
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT `df`.*, ROW_NUMBER() OVER (ORDER BY RAND()) AS `q01`
        FROM `df`
      ) `q01`
      WHERE (`q01` <= 1)

---

    Code
      copy_inline(con_maria, tibble(x = 1:2, y = letters[1:2])) %>% remote_query()
    Output
      <SQL> SELECT CAST(`x` AS INTEGER) AS `x`, CAST(`y` AS CHAR) AS `y`
      FROM (
        SELECT NULL AS `x`, NULL AS `y`
        WHERE (0 = 1)
      
        UNION ALL
      
        VALUES (1, 'a'), (2, 'b')
      ) `values_table`

---

    Code
      copy_inline(con_mysql, tibble(x = 1:2, y = letters[1:2])) %>% remote_query()
    Output
      <SQL> SELECT CAST(`x` AS INTEGER) AS `x`, CAST(`y` AS CHAR) AS `y`
      FROM (
        SELECT NULL AS `x`, NULL AS `y`
        WHERE (0 = 1)
      
        UNION ALL
      
        VALUES ROW(1, 'a'), ROW(2, 'b')
      ) `values_table`

# `sql_query_update_from()` is correct

    Code
      sql_query_update_from(con = con, table = ident("df_x"), from = sql_render(df_y,
        con, lvl = 1), by = c("a", "b"), update_values = sql(c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"))
    Output
      <SQL> UPDATE `df_x`
      INNER JOIN (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        ON `...y`.`a` = `df_x`.`a` AND `...y`.`b` = `df_x`.`b`
      SET `df_x`.`c` = COALESCE(`df_x`.`c`, `...y`.`c`), `df_x`.`d` = `...y`.`d`

---

    Argument `returning_cols` isn't supported in MariaDB translation.

# can explain

    Code
      db %>% mutate(y = x + 1) %>% explain()
    Output
      <SQL>
      SELECT `test`.*, `x` + 1.0 AS `y`
      FROM `test`
      
      <PLAN>
        id select_type table type possible_keys  key key_len  ref rows Extra
      1  1      SIMPLE  test  ALL          <NA> <NA>    <NA> <NA>    3      

