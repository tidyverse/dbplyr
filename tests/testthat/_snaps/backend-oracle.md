# queries translate correctly

    Code
      mf %>% head()
    Output
      <SQL>
      SELECT * FROM (SELECT *
      FROM (`df`) ) `q01` WHERE ROWNUM <= 6.0

# can explain

    Code
      sql_explain(simulate_oracle(), sql("SELECT * FROM foo"))
    Output
      <SQL> EXPLAIN PLAN FOR SELECT * FROM foo;
      SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY()));

