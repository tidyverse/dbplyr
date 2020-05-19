> mf <- lazy_frame(x = 1:3, con = simulate_mssql())

filter and mutate translate is.na correctly
===========================================

> mf %>% head()
<SQL>
SELECT TOP(6) *
FROM `df`

> mf %>% mutate(z = is.na(x))
<SQL>
SELECT `x`, CONVERT(BIT, IIF(`x` IS NULL, 1, 0)) AS `z`
FROM `df`

> mf %>% mutate(z = !is.na(x))
<SQL>
SELECT `x`, ~(CONVERT(BIT, IIF(`x` IS NULL, 1, 0))) AS `z`
FROM `df`

> mf %>% filter(is.na(x))
<SQL>
SELECT *
FROM `df`
WHERE (((`x`) IS NULL))

> # Invalid, can we reliably detect that the return value is logical?
> mf %>% mutate(x = x == 1)
<SQL>
SELECT `x` = 1.0 AS `x`
FROM `df`

> mf %>% mutate(x = x != 1)
<SQL>
SELECT `x` != 1.0 AS `x`
FROM `df`

> mf %>% mutate(x = x > 1)
<SQL>
SELECT `x` > 1.0 AS `x`
FROM `df`

> mf %>% mutate(x = x >= 1)
<SQL>
SELECT `x` >= 1.0 AS `x`
FROM `df`

> mf %>% mutate(x = !(x == 1))
<SQL>
SELECT ~((`x` = 1.0)) AS `x`
FROM `df`

> mf %>% mutate(x = !(x != 1))
<SQL>
SELECT ~((`x` != 1.0)) AS `x`
FROM `df`

> mf %>% mutate(x = !(x > 1))
<SQL>
SELECT ~((`x` > 1.0)) AS `x`
FROM `df`

> mf %>% mutate(x = !(x >= 1))
<SQL>
SELECT ~((`x` >= 1.0)) AS `x`
FROM `df`

> mf %>% mutate(x = x > 4 & x < 5)
<SQL>
SELECT `x` > 4.0 & `x` < 5.0 AS `x`
FROM `df`

> mf %>% filter(x > 4 & x < 5)
<SQL>
SELECT *
FROM `df`
WHERE (`x` > 4.0 AND `x` < 5.0)

> mf %>% mutate(x = x > 4 | x < 5)
<SQL>
SELECT `x` > 4.0 | `x` < 5.0 AS `x`
FROM `df`

> mf %>% filter(x > 4 | x < 5)
<SQL>
SELECT *
FROM `df`
WHERE (`x` > 4.0 OR `x` < 5.0)

> # Work around with explicit ifelse()
> mf %>% mutate(x = ifelse(x == 0, 0, 1))
<SQL>
SELECT CASE WHEN (`x` = 0.0) THEN (0.0) WHEN NOT(`x` = 0.0) THEN (1.0) END AS `x`
FROM `df`


Special ifelse and case_when cases return the correct queries
=============================================================

> mf %>% mutate(z = ifelse(x %in% c(1, 2), 0, 1))
<SQL>
SELECT `x`, CASE WHEN (`x` IN (1.0, 2.0)) THEN (0.0) WHEN NOT(`x` IN (1.0, 2.0)) THEN (1.0) END AS `z`
FROM `df`

> mf %>% mutate(z = case_when(is.na(x) ~ 1, !is.na(x) ~ 2, TRUE ~ 3))
<SQL>
SELECT `x`, CASE
WHEN (((`x`) IS NULL)) THEN (1.0)
WHEN (NOT(((`x`) IS NULL))) THEN (2.0)
ELSE (3.0)
END AS `z`
FROM `df`


ORDER BY in subqueries uses TOP 100 PERCENT (#175)
==================================================

> mf %>% mutate(x = -x) %>% arrange(x) %>% mutate(x = -x)
<SQL>
SELECT -`x` AS `x`
FROM (SELECT TOP 100 PERCENT *
FROM (SELECT TOP 100 PERCENT -`x` AS `x`
FROM `df`) `dbplyr_001`
ORDER BY `x`) `dbplyr_002`

