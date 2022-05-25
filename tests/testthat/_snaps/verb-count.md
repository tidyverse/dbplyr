# generates expected SQL for common situations

    Code
      db %>% count(g)
    Output
      <SQL>
      SELECT `g`, COUNT(*) AS `n`
      FROM `df`
      GROUP BY `g`

---

    Code
      db %>% count(g, wt = x)
    Output
      <SQL>
      SELECT `g`, SUM(`x`) AS `n`
      FROM `df`
      GROUP BY `g`

---

    Code
      db %>% count(g, sort = TRUE)
    Output
      <SQL>
      SELECT `g`, COUNT(*) AS `n`
      FROM `df`
      GROUP BY `g`
      ORDER BY `n` DESC

---

    Code
      db %>% add_count(g, sort = TRUE)
    Output
      <SQL>
      SELECT *, COUNT(*) OVER (PARTITION BY `g`) AS `n`
      FROM `df`
      ORDER BY `n` DESC

---

    Code
      db %>% group_by(g) %>% add_count()
    Output
      <SQL>
      SELECT *, COUNT(*) OVER (PARTITION BY `g`) AS `n`
      FROM `df`

# complains about bad names

    Code
      db <- lazy_frame(g = 1, x = 2)
      db %>% count(g, name = "g")
    Condition
      Error in `tally()`:
      ! 'g' already present in output
      i Use `name = "new_name"` to pick a new name.

# .drop is not supported

    Code
      lazy_frame(g = 1) %>% add_count(.drop = TRUE)
    Condition
      Error in `add_count()`:
      ! `.drop` argument not supported for lazy tables.

