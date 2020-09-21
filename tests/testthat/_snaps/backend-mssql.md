# filter and mutate translate is.na correctly

    Code
      mf %>% mutate(z = is.na(x))
    Output
      <SQL>
      SELECT `x`, CONVERT(BIT, IIF(`x` IS NULL, 1, 0)) AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = !is.na(x))
    Output
      <SQL>
      SELECT `x`, ~(CONVERT(BIT, IIF(`x` IS NULL, 1, 0))) AS `z`
      FROM `df`

---

    Code
      mf %>% filter(is.na(x))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (((`x`) IS NULL))

---

    Code
      mf %>% mutate(x = x == 1)
    Output
      <SQL>
      SELECT `x` = 1.0 AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x != 1)
    Output
      <SQL>
      SELECT `x` != 1.0 AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x > 1)
    Output
      <SQL>
      SELECT `x` > 1.0 AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x >= 1)
    Output
      <SQL>
      SELECT `x` >= 1.0 AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = !(x == 1))
    Output
      <SQL>
      SELECT ~((`x` = 1.0)) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = !(x != 1))
    Output
      <SQL>
      SELECT ~((`x` != 1.0)) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = !(x > 1))
    Output
      <SQL>
      SELECT ~((`x` > 1.0)) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = !(x >= 1))
    Output
      <SQL>
      SELECT ~((`x` >= 1.0)) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x > 4 & x < 5)
    Output
      <SQL>
      SELECT `x` > 4.0 & `x` < 5.0 AS `x`
      FROM `df`

---

    Code
      mf %>% filter(x > 4 & x < 5)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` > 4.0 AND `x` < 5.0)

---

    Code
      mf %>% mutate(x = x > 4 | x < 5)
    Output
      <SQL>
      SELECT `x` > 4.0 | `x` < 5.0 AS `x`
      FROM `df`

---

    Code
      mf %>% filter(x > 4 | x < 5)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` > 4.0 OR `x` < 5.0)

---

    Code
      mf %>% mutate(x = ifelse(x == 0, 0, 1))
    Output
      <SQL>
      SELECT CASE WHEN (`x` = 0.0) THEN (0.0) WHEN NOT(`x` = 0.0) THEN (1.0) END AS `x`
      FROM `df`

# Special ifelse and case_when cases return the correct queries

    Code
      mf %>% mutate(z = ifelse(x %in% c(1, 2), 0, 1))
    Output
      <SQL>
      SELECT `x`, CASE WHEN (`x` IN (1.0, 2.0)) THEN (0.0) WHEN NOT(`x` IN (1.0, 2.0)) THEN (1.0) END AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = case_when(is.na(x) ~ 1, !is.na(x) ~ 2, TRUE ~ 3))
    Output
      <SQL>
      SELECT `x`, CASE
      WHEN (((`x`) IS NULL)) THEN (1.0)
      WHEN (NOT(((`x`) IS NULL))) THEN (2.0)
      ELSE (3.0)
      END AS `z`
      FROM `df`

# ORDER BY in subqueries uses TOP 9223372036854775807 (#337)

    Code
      sql_select(simulate_mssql(), "x", "y", order_by = "z", bare_identifier_ok = TRUE)
    Output
      <SQL> SELECT TOP 9223372036854775807 'x'
      FROM 'y'
      ORDER BY 'z'

# custom escapes translated correctly

    Code
      mf %>% filter(x == a)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = 0x616263)

---

    Code
      mf %>% filter(x %in% L)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (0x616263, 0x0102))

---

    Code
      qry
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` IN (0x616263, 0x0102))

