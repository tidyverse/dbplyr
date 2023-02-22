# select after distinct produces subquery

    Code
      lf %>% distinct() %>% select(x)
    Output
      <SQL>
      SELECT `x`
      FROM (
        SELECT DISTINCT *
        FROM `df`
      ) `q01`

# rename/relocate after distinct is inlined #1141

    Code
      lf %>% distinct() %>% rename(z = y)
    Output
      <SQL>
      SELECT DISTINCT `x`, `y` AS `z`
      FROM `df`
    Code
      lf %>% distinct() %>% relocate(y)
    Output
      <SQL>
      SELECT DISTINCT `y`, `x`
      FROM `df`

# select preserves grouping vars

    Code
      out <- mf %>% select(a) %>% collect()
    Message
      Adding missing grouping variables: `b`

# select() after left_join() is inlined

    Code
      (out <- left_join(lf1, lf2, by = "x") %>% select(b, x))
    Output
      <SQL>
      SELECT `b`, `lf1`.`x` AS `x`
      FROM `lf1`
      LEFT JOIN `lf2`
        ON (`lf1`.`x` = `lf2`.`x`)

---

    Code
      (out <- left_join(lf1, lf2, by = "x") %>% relocate(b))
    Output
      <SQL>
      SELECT `b`, `lf1`.*
      FROM `lf1`
      LEFT JOIN `lf2`
        ON (`lf1`.`x` = `lf2`.`x`)

# select() after semi_join() is inlined

    Code
      (out <- semi_join(lf1, lf2, by = "x") %>% select(x, a2 = a))
    Output
      <SQL>
      SELECT `x`, `a` AS `a2`
      FROM `lf1`
      WHERE EXISTS (
        SELECT 1 FROM `lf2`
        WHERE (`lf1`.`x` = `lf2`.`x`)
      )

---

    Code
      (out <- anti_join(lf1, lf2, by = "x") %>% relocate(a))
    Output
      <SQL>
      SELECT `a`, `x`
      FROM `lf1`
      WHERE NOT EXISTS (
        SELECT 1 FROM `lf2`
        WHERE (`lf1`.`x` = `lf2`.`x`)
      )

# select() after join handles previous select

    Code
      print(lf)
    Output
      <SQL>
      SELECT `x` AS `x2`, `y` AS `y3`, `z`
      FROM `df` AS `df_LHS`
      WHERE EXISTS (
        SELECT 1 FROM `df` AS `df_RHS`
        WHERE (`df_LHS`.`x` = `df_RHS`.`x`)
      )

---

    Code
      print(lf2)
    Output
      <SQL>
      SELECT `df_LHS`.`x` AS `x2`, `df_LHS`.`y` AS `y3`, `z`
      FROM `df` AS `df_LHS`
      LEFT JOIN `df` AS `df_RHS`
        ON (`df_LHS`.`x` = `df_RHS`.`x`)

# select() produces nice error messages

    Code
      lf %>% select(non_existent)
    Condition
      Error in `select()`:
      ! Can't subset columns that don't exist.
      x Column `non_existent` doesn't exist.
    Code
      lf %>% select(non_existent + 1)
    Condition
      Error in `select()`:
      ! Problem while evaluating `non_existent + 1`.
      Caused by error:
      ! object 'non_existent' not found

---

    Code
      lf %>% relocate(non_existent)
    Condition
      Error in `relocate()`:
      ! Can't subset columns that don't exist.
      x Column `non_existent` doesn't exist.
    Code
      lf %>% relocate(non_existent + 1)
    Condition
      Error in `relocate()`:
      ! Problem while evaluating `non_existent + 1`.
      Caused by error:
      ! object 'non_existent' not found

---

    Code
      lf %>% rename(x)
    Condition
      Error in `rename()`:
      ! All renaming inputs must be named.
    Code
      lf %>% rename(y = non_existent)
    Condition
      Error in `rename()`:
      ! Can't rename columns that don't exist.
      x Column `non_existent` doesn't exist.
    Code
      lf %>% rename(y = non_existent + 1)
    Condition
      Error in `rename()`:
      ! Problem while evaluating `non_existent + 1`.
      Caused by error:
      ! object 'non_existent' not found

---

    Code
      lf %>% rename_with(toupper, .cols = non_existent)
    Condition
      Error in `rename_with()`:
      ! Can't subset columns that don't exist.
      x Column `non_existent` doesn't exist.
    Code
      lf %>% rename_with(toupper, .cols = non_existent + 1)
    Condition
      Error in `rename_with()`:
      ! Problem while evaluating `non_existent + 1`.
      Caused by error:
      ! object 'non_existent' not found

# where() isn't suppored

    Code
      lf %>% select(where(is.integer))
    Condition
      Error in `select()`:
      ! This tidyselect interface doesn't support predicates.

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
        `df`.`y`[34m AS [39m`y.y`,
        `df`.`z`[34m AS [39m`z.y`
      [34mFROM[39m `q02`[34m AS [39m`LHS`
      [34mLEFT JOIN[39m `df`
        [34mON[39m (`LHS`.`x` = `df`.`x`)

