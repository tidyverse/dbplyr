
mutate collapses over nested select
===================================

> lf <- lazy_frame(g = 0, x = 1, y = 2)
> # a
> lf %>% mutate(a = 1, b = 2) %>% select(a)
<SQL>
SELECT 1.0 AS `a`
FROM `df`

> # x
> lf %>% mutate(a = 1, b = 2) %>% select(x)
<SQL>
SELECT `x`
FROM `df`

