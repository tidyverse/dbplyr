# queries translate correctly

    Code
      mf %>% head()
    Output
      <SQL>
      SELECT * FROM (SELECT *
      FROM (`df`) ) `q01` WHERE ROWNUM <= 6.0

# generates custom sql

    Code
      sql_analyze(con, ident("table"))
    Output
      <SQL> ANALYZE TABLE `table` COMPUTE STATISTICS

---

    Code
      sql_explain(con, sql("SELECT * FROM foo"))
    Output
      <SQL> EXPLAIN PLAN FOR SELECT * FROM foo;
      SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY()));

---

    Code
      sql_drop_table(con, ident("table"))
    Output
      <SQL> DROP TABLE `table`

---

    Code
      sql_drop_table(con, ident("table"), force = TRUE)
    Output
      <SQL> BEGIN EXECUTE IMMEDIATE 'DROP TABLE `table`';
      EXCEPTION WHEN OTHERS THEN IF SQLCODE != -942 THEN RAISE; END IF;
      END;

