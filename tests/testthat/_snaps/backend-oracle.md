# queries translate correctly

    Code
      mf %>% head()
    Output
      <SQL>
      SELECT * FROM (SELECT *
      FROM (`df`) ) `zzz2` WHERE ROWNUM <= 6.0

