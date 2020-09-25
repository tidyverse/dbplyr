# generates custom sql

    Code
      sql_table_analyze(con, ident("table"))
    Output
      <SQL> COLLECT STATISTICS `table`

# head translated to TOP

    Code
      mf %>% head() %>% sql_render()
    Output
      <SQL> SELECT TOP 6 *
      FROM `df`

