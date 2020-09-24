# queries translate correctly

    Code
      mf %>% head()
    Output
      <SQL>
      SELECT * FROM (SELECT *
      FROM (`df`) ) `q01` WHERE ROWNUM <= 6.0

# generates custom sql

    Code
      sql_table_analyze(con, ident("table"))
    Output
      <SQL> ANALYZE TABLE `table` COMPUTE STATISTICS

---

    Code
      sql_query_explain(con, sql("SELECT * FROM foo"))
    Output
      <SQL> EXPLAIN PLAN FOR SELECT * FROM foo;
      SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY()));

