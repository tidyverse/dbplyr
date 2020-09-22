# convert between bit and boolean as needed

    Code
      mf %>% filter(is.na(x))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (((`x`) IS NULL))

---

    Code
      mf %>% filter(!is.na(x))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (NOT(((`x`) IS NULL)))

---

    Code
      mf %>% filter(x == 1L || x == 2L)
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`x` = 1 OR `x` = 2)

---

    Code
      mf %>% mutate(z = ifelse(x == 1L, 1L, 2L))
    Output
      <SQL>
      SELECT `x`, IIF(`x` = 1, 1, 2) AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = case_when(x == 1L ~ 1L))
    Output
      <SQL>
      SELECT `x`, CASE
      WHEN (`x` = 1) THEN (1)
      END AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(z = !is.na(x))
    Output
      <SQL>
      SELECT `x`, IIF(~((`x`) IS NULL), 1, 0) AS `z`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L)
    Output
      <SQL>
      SELECT IIF(`x` = 1, 1, 0) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L || x == 2L)
    Output
      <SQL>
      SELECT IIF(`x` = 1 OR `x` = 2, 1, 0) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = x == 1L || x == 2L || x == 3L)
    Output
      <SQL>
      SELECT IIF(`x` = 1 OR `x` = 2 OR `x` = 3, 1, 0) AS `x`
      FROM `df`

---

    Code
      mf %>% mutate(x = !(x == 1L || x == 2L || x == 3L))
    Output
      <SQL>
      SELECT IIF(~(`x` = 1 OR `x` = 2 OR `x` = 3), 1, 0) AS `x`
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

