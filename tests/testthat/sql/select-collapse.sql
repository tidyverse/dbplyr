
multiple selects are collapsed
==============================

> lf <- lazy_frame(x = 1, y = 2)
> # flip two times
> lf %>% select(2:1) %>% select(2:1)
<SQL>
SELECT `x`, `y`
FROM `df`

> # flip three times
> lf %>% select(2:1) %>% select(2:1) %>% select(2:1)
<SQL>
SELECT `y`, `x`
FROM `df`

> # rename
> lf %>% select(x1 = x) %>% select(x2 = x1)
<SQL>
SELECT `x` AS `x2`
FROM `df`

