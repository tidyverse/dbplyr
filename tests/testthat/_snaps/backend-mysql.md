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
      slice_sample(lf, n = 1)
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT *, ROW_NUMBER() OVER (ORDER BY RAND()) AS `q01`
        FROM `df`
      ) `q01`
      WHERE (`q01` <= 1)

---

    Code
      copy_inline(con, tibble(x = 1:2, y = letters[1:2])) %>% remote_query()
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

# `sql_query_update_from()` is correct

    Code
      sql_query_update_from(con = simulate_mysql(), x_name = ident("df_x"), y = df_y,
      by = c("a", "b"), update_values = sql(c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> UPDATE `df_x`
      INNER JOIN (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        ON `...y`.`a` = `df_x`.`a` AND `...y`.`b` = `df_x`.`b`
      SET `df_x`.`c` = COALESCE(`df_x`.`c`, `...y`.`c`), `df_x`.`d` = `...y`.`d`
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

