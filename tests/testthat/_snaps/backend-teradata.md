# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> COLLECT STATISTICS `schema`.`tbl`

# head translated to TOP

    Code
      mf %>% head() %>% sql_render()
    Output
      <SQL> SELECT TOP 6 *
      FROM `df`

