# queries translate correctly

    Code
      mf %>% head()
    Output
      <SQL>
      SELECT * FROM (SELECT *
      FROM (`df`) ) `q01` WHERE ROWNUM <= 6.0

