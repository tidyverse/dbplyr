# select preserves grouping vars

    Code
      out <- mf %>% select(a) %>% collect()
    Message
      Adding missing grouping variables: `b`

# multiple selects are collapsed

    Code
      lf %>% select(2:1) %>% select(2:1)
    Output
      <SQL>
      SELECT *
      FROM `df`

---

    Code
      lf %>% select(2:1) %>% select(2:1) %>% select(2:1)
    Output
      <SQL>
      SELECT `y`, `x`
      FROM `df`

---

    Code
      lf %>% select(x1 = x) %>% select(x2 = x1)
    Output
      <SQL>
      SELECT `x` AS `x2`
      FROM `df`

# mutate collapses over nested select

    Code
      lf %>% mutate(a = 1, b = 2) %>% select(a)
    Output
      <SQL>
      SELECT 1.0 AS `a`
      FROM `df`

---

    Code
      lf %>% mutate(a = 1, b = 2) %>% select(x)
    Output
      <SQL>
      SELECT `x`
      FROM `df`

# output is styled

    Code
      show_query(out, cte = TRUE)
    Output
      <SQL>
      [34mWITH [39m`q01`[34m AS[39m (
        [34mSELECT[39m `x`, AVG(`y`) OVER (PARTITION BY `x`)[34m AS [39m`y`, `z` + 1.0[34m AS [39m`z`
        [34mFROM[39m `df`
      ),
      `q02`[34m AS[39m (
        [34mSELECT[39m *
        [34mFROM[39m `q01`
        [34mWHERE[39m (`z` = 1.0)
      )
      [34mSELECT[39m
        `LHS`.`x`[34m AS [39m`x`,
        `LHS`.`y`[34m AS [39m`y.x`,
        `LHS`.`z`[34m AS [39m`z.x`,
        `RHS`.`y`[34m AS [39m`y.y`,
        `RHS`.`z`[34m AS [39m`z.y`
      [34mFROM[39m `q02`[34m AS [39m`LHS`
      [34mLEFT JOIN[39m `df`[34m AS [39m`RHS`
        [34mON[39m (`LHS`.`x` = `RHS`.`x`)

