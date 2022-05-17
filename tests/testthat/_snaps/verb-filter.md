# errors for named input

    Code
      filter(lf, x = 1)
    Condition
      Error in `filter()`:
      ! Problem with `filter()` input `..1`.
      x Input `..1` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

---

    Code
      filter(lf, y > 1, x = 1)
    Condition
      Error in `filter()`:
      ! Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

# .preserve is not supported

    Code
      lf %>% filter(x == 1, .preserve = TRUE)
    Condition
      Error in `filter()`:
      ! `.preserve` is not supported on database backends

# filter() after summarise() uses `HAVING`

    Code
      (out <- lf %>% filter(g == 1))
    Message
      `summarise()` has grouped output by "g". You can override using the `.groups` argument.
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`
      HAVING `g` = 1.0

---

    Code
      (out <- lf %>% filter(x_mean > 1))
    Message
      `summarise()` has grouped output by "g". You can override using the `.groups` argument.
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`
      HAVING AVG(`x`) > 1.0

---

    Code
      (out <- lf %>% filter(g == 1) %>% filter(g == 2))
    Message
      `summarise()` has grouped output by "g". You can override using the `.groups` argument.
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`
      HAVING `g` = 1.0, `g` = 2.0

---

    Code
      (out <- lf %>% filter(g == 1) %>% filter(h == 2))
    Message
      `summarise()` has grouped output by "g". You can override using the `.groups` argument.
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`
      HAVING `g` = 1.0, `h` = 2.0

