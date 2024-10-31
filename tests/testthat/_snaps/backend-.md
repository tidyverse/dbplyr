# basic arithmetic is correct

    Code
      test_translate_sql(100L %/% 3L)
    Condition
      Error in `100L %/% 3L`:
      ! `%/%()` is not available in this SQL variant.

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
    Output
      <SQL>
      SELECT `df`.*
      FROM `df`
      WHERE (`x` = 1.0)

# useful error if $ used with inlined value

    Code
      lazy_frame(x = 1) %>% filter(x == y$id)
    Condition
      Error in `x$id`:
      ! $ operator is invalid for atomic vectors

# can only translate case sensitive str_like

    Code
      test_translate_sql(str_like(x, "abc", ignore_case = TRUE))
    Condition
      Error in `str_like()`:
      ! Backend does not support case insensitive `str_like()`.
      i Set `ignore_case = FALSE` for case sensitive match.
      i Use `tolower()` on both arguments to achieve a case insensitive match.

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
      <table_path> `table`

---

    Code
      sql_query_wrap(con, in_schema("schema", "tbl"))
    Output
      <table_path> `schema`.`tbl`

---

    Code
      sql_query_wrap(con, sql("SELECT * FROM foo"))
    Output
      <SQL> (SELECT * FROM foo) AS `q01`

---

    Code
      sql_table_index(con, in_schema("schema", "tbl"), c("a", "b"))
    Output
      <SQL> CREATE INDEX `tbl_a_b` ON `schema`.`tbl` (`a`, `b`)

---

    Code
      sql_table_index(con, in_schema("schema", "tbl"), "c", unique = TRUE)
    Output
      <SQL> CREATE UNIQUE INDEX `tbl_c` ON `schema`.`tbl` (`c`)

---

    Code
      sql_query_save(con, sql("SELECT * FROM foo"), in_schema("temp", "tbl"))
    Output
      <SQL> CREATE TEMPORARY TABLE 
      `temp`.`tbl` AS
      SELECT * FROM foo

