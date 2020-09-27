# generates custom sql

    Code
      sql_table_analyze(simulate_hive(), ident("tbl"))
    Output
      <SQL> ANALYZE TABLE `tbl` COMPUTE STATISTICS

