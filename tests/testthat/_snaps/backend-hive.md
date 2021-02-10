# generates custom sql

    Code
      sql_table_analyze(simulate_hive(), in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE `schema`.`tbl` COMPUTE STATISTICS

