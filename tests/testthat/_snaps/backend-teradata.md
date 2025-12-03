# generates custom sql

    Code
      sql_table_analyze(con, in_schema("schema", "tbl"))
    Output
      <SQL> COLLECT STATISTICS `schema`.`tbl`

# head translated to TOP

    Code
      sql_render(head(mf))
    Output
      <SQL> SELECT TOP 6 *
      FROM `df`

# lead, lag work

    Code
      sql_render(mutate(group_by(mf, y), val2 = lead(x, order_by = x)))
    Output
      <SQL> SELECT *, LEAD(`x`, 1, NULL) OVER (PARTITION BY `y` ORDER BY `x`) AS `val2`
      FROM `df`

---

    Code
      sql_render(mutate(group_by(mf, y), val2 = lag(x, order_by = x)))
    Output
      <SQL> SELECT *, LAG(`x`, 1, NULL) OVER (PARTITION BY `y` ORDER BY `x`) AS `val2`
      FROM `df`

# weighted.mean

    Code
      summarise(mf, wt_mean = weighted.mean(x, y))
    Output
      <SQL>
      SELECT SUM((`x` * `y`)) / SUM(`y`) OVER () AS `wt_mean`
      FROM `df`

# row_number() with and without group_by() and arrange(): unordered defaults to Ordering by NULL (per empty_order)

    Code
      mutate(mf, rown = row_number())
    Output
      <SQL>
      SELECT *, ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS `rown`
      FROM `df`

---

    Code
      mutate(group_by(mf, y), rown = row_number())
    Output
      <SQL>
      SELECT *, ROW_NUMBER() OVER (PARTITION BY `y` ORDER BY (SELECT NULL)) AS `rown`
      FROM `df`

---

    Code
      mutate(arrange(mf, y), rown = row_number())
    Output
      <SQL>
      SELECT *, ROW_NUMBER() OVER (ORDER BY `y`) AS `rown`
      FROM `df`
      ORDER BY `y`

# head after distinct() produces subquery

    Code
      head(distinct(lf))
    Output
      <SQL>
      SELECT TOP 6 `q01`.*
      FROM (
        SELECT DISTINCT *
        FROM `df`
      ) AS `q01`

