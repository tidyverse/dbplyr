
arrange renders correctly
=========================

> lf <- lazy_frame(a = 1:3, b = 3:1)
> # basic
> lf %>% arrange(a)
<SQL>
SELECT *
FROM `df`
ORDER BY `a`

> # double arrange
> lf %>% arrange(a) %>% arrange(b)
<SQL>
SELECT *
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_001`
ORDER BY `b`

> # remove ordered by
> lf %>% arrange(a) %>% select(-a)
<SQL>
SELECT `b`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_002`

> lf %>% arrange(a) %>% select(-a) %>% arrange(b)
<SQL>
SELECT `b`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_003`
ORDER BY `b`

> # un-arrange
> lf %>% arrange(a) %>% arrange()
<SQL>
SELECT *
FROM `df`
ORDER BY `a`

> lf %>% arrange(a) %>% select(-a) %>% arrange()
<SQL>
SELECT `b`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_004`

> # use order
> lf %>% arrange(a) %>% select(-a) %>% mutate(c = lag(b))
<SQL>
SELECT `b`, LAG(`b`, 1, NULL) OVER (ORDER BY `a`) AS `c`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_005`

> # head
> lf %>% head(1) %>% arrange(a)
<SQL>
SELECT *
FROM (SELECT *
FROM `df`
LIMIT 1) `dbplyr_006`
ORDER BY `a`

> lf %>% arrange(a) %>% head(1)
<SQL>
SELECT *
FROM `df`
ORDER BY `a`
LIMIT 1

> lf %>% arrange(a) %>% head(1) %>% arrange(b)
<SQL>
SELECT *
FROM (SELECT *
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_007`
LIMIT 1) `dbplyr_008`
ORDER BY `b`

> # mutate
> lf %>% mutate(a = b) %>% arrange(a)
<SQL>
SELECT `b` AS `a`, `b`
FROM `df`
ORDER BY `a`

> # complex mutate
> lf %>% arrange(a) %>% mutate(a = b) %>% arrange(a)
<SQL>
SELECT `b` AS `a`, `b`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_009`
ORDER BY `a`

> lf %>% arrange(a) %>% mutate(a = 1) %>% arrange(b)
<SQL>
SELECT 1.0 AS `a`, `b`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_010`
ORDER BY `b`

> lf %>% arrange(a) %>% mutate(b = a) %>% arrange(b)
<SQL>
SELECT `a`, `a` AS `b`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_011`
ORDER BY `b`

> lf %>% arrange(a) %>% mutate(b = 1) %>% arrange(b)
<SQL>
SELECT `a`, 1.0 AS `b`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `dbplyr_012`
ORDER BY `b`

> lf %>% mutate(a = -a) %>% arrange(a) %>% mutate(a = -a)
<SQL>
SELECT -`a` AS `a`, `b`
FROM (SELECT *
FROM (SELECT -`a` AS `a`, `b`
FROM `df`) `dbplyr_013`
ORDER BY `a`) `dbplyr_014`

> # join
> rf <- lazy_frame(a = 1:3, c = 4:6)
> lf %>% arrange(a) %>% left_join(rf)
Message: Joining, by = "a"

<SQL>
SELECT `LHS`.`a` AS `a`, `LHS`.`b` AS `b`, `RHS`.`c` AS `c`
FROM (SELECT *
FROM `df`
ORDER BY `a`) `LHS`
LEFT JOIN `df` AS `RHS`
ON (`LHS`.`a` = `RHS`.`a`)


> lf %>% arrange(b) %>% left_join(rf)
Message: Joining, by = "a"

<SQL>
SELECT `LHS`.`a` AS `a`, `LHS`.`b` AS `b`, `RHS`.`c` AS `c`
FROM (SELECT *
FROM `df`
ORDER BY `b`) `LHS`
LEFT JOIN `df` AS `RHS`
ON (`LHS`.`a` = `RHS`.`a`)


> lf %>% left_join(rf %>% arrange(a))
Message: Joining, by = "a"

<SQL>
SELECT `LHS`.`a` AS `a`, `LHS`.`b` AS `b`, `RHS`.`c` AS `c`
FROM `df` AS `LHS`
LEFT JOIN (SELECT *
FROM `df`
ORDER BY `a`) `RHS`
ON (`LHS`.`a` = `RHS`.`a`)


> lf %>% left_join(rf %>% arrange(c))
Message: Joining, by = "a"

<SQL>
SELECT `LHS`.`a` AS `a`, `LHS`.`b` AS `b`, `RHS`.`c` AS `c`
FROM `df` AS `LHS`
LEFT JOIN (SELECT *
FROM `df`
ORDER BY `c`) `RHS`
ON (`LHS`.`a` = `RHS`.`a`)


> lf %>% arrange(a) %>% semi_join(rf)
Message: Joining, by = "a"

<SQL>
SELECT * FROM (SELECT *
FROM `df`
ORDER BY `a`) `LHS`
WHERE EXISTS (
  SELECT 1 FROM `df` AS `RHS`
  WHERE (`LHS`.`a` = `RHS`.`a`)
)

> lf %>% arrange(b) %>% semi_join(rf)
Message: Joining, by = "a"

<SQL>
SELECT * FROM (SELECT *
FROM `df`
ORDER BY `b`) `LHS`
WHERE EXISTS (
  SELECT 1 FROM `df` AS `RHS`
  WHERE (`LHS`.`a` = `RHS`.`a`)
)

> lf %>% semi_join(rf %>% arrange(a))
Message: Joining, by = "a"

<SQL>
SELECT * FROM `df` AS `LHS`
WHERE EXISTS (
  SELECT 1 FROM (SELECT *
FROM `df`
ORDER BY `a`) `RHS`
  WHERE (`LHS`.`a` = `RHS`.`a`)
)

> lf %>% semi_join(rf %>% arrange(c))
Message: Joining, by = "a"

<SQL>
SELECT * FROM `df` AS `LHS`
WHERE EXISTS (
  SELECT 1 FROM (SELECT *
FROM `df`
ORDER BY `c`) `RHS`
  WHERE (`LHS`.`a` = `RHS`.`a`)
)

