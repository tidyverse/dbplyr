# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE `schema`.`tbl` COMPUTE STATISTICS

---

    Code
      union_all(lf, lf)
    Output
      <SQL>
      SELECT *
      FROM `df`
      
      UNION ALL
      
      SELECT *
      FROM `df`

