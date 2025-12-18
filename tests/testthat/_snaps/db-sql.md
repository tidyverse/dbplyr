# analyse/explain sql generates expected SQL

    Code
      sql_table_analyze(con, "tbl")
    Output
      <SQL> ANALYZE "tbl"

---

    Code
      sql_query_explain(con, sql("SELECT * FROM foo"))
    Output
      <SQL> EXPLAIN SELECT * FROM foo

# sql_query_wrap generates expected SQL

    Code
      sql_query_wrap(con, ident("table"))
    Output
      <table_path> "table"

---

    Code
      sql_query_wrap(con, in_schema("schema", "tbl"))
    Output
      <table_path> "schema"."tbl"

---

    Code
      sql_query_wrap(con, sql("SELECT * FROM foo"))
    Output
      <SQL> (SELECT * FROM foo) AS "q04"

# sql_table_index generates expected SQL

    Code
      sql_table_index(con, "tbl", c("a", "b"))
    Output
      <SQL> CREATE INDEX "tbl_a_b" ON "tbl" ("a", "b")

---

    Code
      sql_table_index(con, "tbl", "c", unique = TRUE)
    Output
      <SQL> CREATE UNIQUE INDEX "tbl_c" ON "tbl" ("c")

# sql_query_save generates expected SQL

    Code
      sql_query_save(con, sql, "tbl")
    Output
      <SQL> CREATE TEMPORARY TABLE
      "tbl" AS
      SELECT * FROM foo

---

    Code
      sql_query_save(con, sql, "tbl", temporary = FALSE)
    Output
      <SQL> CREATE TABLE
      "tbl" AS
      SELECT * FROM foo

