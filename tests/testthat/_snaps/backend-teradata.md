# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> COLLECT STATISTICS `schema`.`tbl`

# head translated to TOP

    Code
      mf %>% head() %>% sql_render()
    Output
      <SQL> SELECT TOP 6 `df`.*
      FROM `df`

# lead, lag work

    Code
      mf %>% group_by(y) %>% mutate(val2 = lead(x, order_by = x)) %>% sql_render()
    Output
      <SQL> SELECT `df`.*, LEAD(`x`, 1, NULL) OVER (PARTITION BY `y` ORDER BY `x`) AS `val2`
      FROM `df`

---

    Code
      mf %>% group_by(y) %>% mutate(val2 = lag(x, order_by = x)) %>% sql_render()
    Output
      <SQL> SELECT `df`.*, LAG(`x`, 1, NULL) OVER (PARTITION BY `y` ORDER BY `x`) AS `val2`
      FROM `df`

# weighted.mean

    Code
      mf %>% summarise(wt_mean = weighted.mean(x, y))
    Output
      <SQL>
      SELECT SUM((`x` * `y`)) / SUM(`y`) OVER () AS `wt_mean`
      FROM `df`

# row_number with and without group_by

    Code
      mf %>% mutate(rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS `rown`
      FROM `df`

---

    Code
      mf %>% group_by(y) %>% mutate(rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER (PARTITION BY `y` ORDER BY `y`) AS `rown`
      FROM `df`

# head after distinct() produces subquery

    Code
      lf %>% distinct() %>% head()
    Output
      <SQL>
      SELECT TOP 6 `q01`.*
      FROM (
        SELECT DISTINCT `df`.*
        FROM `df`
      ) AS `q01`

