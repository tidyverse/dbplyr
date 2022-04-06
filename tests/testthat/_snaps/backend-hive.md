# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> ANALYZE TABLE `schema`.`tbl` COMPUTE STATISTICS

---

    Code
      last_value_sql(con, ident("a"))
    Output
      <SQL> last_value(`a`, TRUE)

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

