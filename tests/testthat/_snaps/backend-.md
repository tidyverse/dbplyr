# can translate case insensitive like

    Code
      translate_sql(str_like(x, "abc", ignore_case = FALSE))
    Condition
      Error in `str_like()`:
      ! Backend only supports case insensitve `str_like()`.

# default raw escapes translated correctly

    Code
      mf %>% filter(x == a)
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` = X'616263')

---

    Code
      mf %>% filter(x %in% L)
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` IN (X'616263', X'0102'))

---

    Code
      qry
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` IN (X'616263', X'0102'))

# DDL operations generate expected SQL

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE `schema`.`tbl`

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
      sql_query_wrap(con, in_schema("schema", "tbl"))
    Output
      <IDENT> `schema`.`tbl`

---

    Code
      sql_query_wrap(con, sql("SELECT * FROM foo"))
    Output
      <SQL> (SELECT * FROM foo) `q01`

---

    Code
      sql_table_index(con, in_schema("schema", "tbl"), c("a", "b"))
    Output
      <SQL> CREATE INDEX `schema_tbl_a_b` ON `schema`.`tbl` (`a`, `b`)

---

    Code
      sql_table_index(con, in_schema("schema", "tbl"), "c", unique = TRUE)
    Output
      <SQL> CREATE UNIQUE INDEX `schema_tbl_c` ON `schema`.`tbl` (`c`)

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("temp", "tbl"))
    Output
      <SQL> CREATE TEMPORARY TABLE 
      `temp`.`tbl` AS
      SELECT * FROM foo

