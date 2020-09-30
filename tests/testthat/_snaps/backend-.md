# default raw escapes translated correctly

    Code
      mf %>% filter(x == a)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = X'616263')

---

    Code
      mf %>% filter(x %in% L)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (X'616263', X'0102'))

---

    Code
      qry
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (X'616263', X'0102'))

# DDL operations generate expected SQL

    Code
      sql_table_analyze(con, ident("table"))
    Output
      <SQL> ANALYZE `table`

---

    Code
      sql_query_explain(con, sql("SELECT * FROM foo"))
    Output
      <SQL> EXPLAIN SELECT * FROM foo

---

    Code
      sql_query_wrap(con, ident("table"))
    Output
      <IDENT> table

---

    Code
      sql_query_wrap(con, sql("SELECT * FROM foo"))
    Output
      <SQL> (SELECT * FROM foo) `q02`

---

    Code
      sql_table_index(con, ident("table"), c("a", "b"))
    Output
      <SQL> CREATE INDEX `table_a_b` ON `table` (`a`, `b`)

---

    Code
      sql_table_index(con, ident("table"), "c", unique = TRUE)
    Output
      <SQL> CREATE UNIQUE INDEX `table_c` ON `table` (`c`)

---

    Code
      sql_query_save(con, ident("table"), sql("SELECT * FROM foo"))
    Output
      <SQL> CREATE TEMPORARY TABLE 
      SELECT * FROM foo AS `table`

