# can refer to fresly created values

    Code
      show_query(out2)
    Output
      <SQL>
      SELECT `x` + 4.0 AS `x`
      FROM (
        SELECT `x` + 2.0 AS `x`
        FROM (
          SELECT `x` + 1.0 AS `x`
          FROM `multi_mutate`
        )
      )

# transmute includes all needed variables

    Code
      out
    Output
      <SQL>
      SELECT `x`, `x` + `y` AS `x2`
      FROM (
        SELECT `x` / 2.0 AS `x`, `y`
        FROM `df`
      ) `q01`

# across() does not select grouping variables

    Code
      df %>% group_by(g) %>% mutate(across(.fns = ~0))
    Output
      <SQL>
      SELECT `g`, 0.0 AS `x`
      FROM `df`

---

    Code
      df %>% group_by(g) %>% transmute(across(.fns = ~0))
    Output
      <SQL>
      SELECT 0.0 AS `x`
      FROM `df`

# mutate generates subqueries as needed

    Code
      lf %>% mutate(x = x + 1, x = x + 1)
    Output
      <SQL>
      SELECT `x` + 1.0 AS `x`
      FROM (
        SELECT `x` + 1.0 AS `x`
        FROM `df`
      )

---

    Code
      lf %>% mutate(x1 = x + 1, x2 = x1 + 1)
    Output
      <SQL>
      SELECT `x`, `x1`, `x1` + 1.0 AS `x2`
      FROM (
        SELECT `x`, `x` + 1.0 AS `x1`
        FROM `df`
      )

# mutate collapses over nested select

    Code
      lf %>% select(x:y) %>% mutate(x = x * 2, y = y * 2)
    Output
      <SQL>
      SELECT `x` * 2.0 AS `x`, `y` * 2.0 AS `y`
      FROM `df`

---

    Code
      lf %>% select(y:x) %>% mutate(x = x * 2, y = y * 2)
    Output
      <SQL>
      SELECT `y` * 2.0 AS `y`, `x` * 2.0 AS `x`
      FROM `df`

