# can translate subsetting

    Code
      test_translate_sql(a[[x]])
    Condition
      Error in `a[[x]]`:
      ! Can only index with strings and numbers
    Code
      test_translate_sql(a[[TRUE]])
    Condition
      Error in `a[[TRUE]]`:
      ! Can only index with strings and numbers

# $ doesn't evaluate second argument

    Code
      lazy_frame(x = 1, y = 1) %>% filter(x == y$id)
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` = `y`.`id`)

---

    Code
      lazy_frame(x = 1) %>% filter(x == y$id)
    Condition
      Error in `filter()`:
      i In argument: `x == y$id`
      Caused by error:
      ! Cannot translate a list to SQL.
      i Do you want to force evaluation in R with (e.g.) `!!y` or `local(y)`?

# useful error if $ used with inlined value

    Code
      lazy_frame(x = 1) %>% filter(x == y$id)
    Condition
      Error in `1$id`:
      ! `$` can only subset database columns, not inlined values.

# can translate case insensitive like

    Code
      test_translate_sql(str_like(x, "abc", ignore_case = FALSE))
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
      <dbplyr_table_ident[1]>
      [1] `table`

---

    Code
      sql_query_wrap(con, in_schema("schema", "tbl"))
    Output
      <dbplyr_table_ident[1]>
      [1] `schema`.`tbl`

---

    Code
      sql_query_wrap(con, sql("SELECT * FROM foo"))
    Output
      <SQL> (SELECT * FROM foo) AS `q01`

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

