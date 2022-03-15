# two filters equivalent to one

    Code
      df1 %>% remote_query()
    Output
      <SQL> SELECT *
      FROM `df`
      WHERE (`x` > 3.0) AND (`y` < 3.0)

---

    Code
      df1 %>% remote_query()
    Output
      <SQL> SELECT `x`, `y`
      FROM (
        SELECT `x`, `y`, AVG(`x`) OVER () AS `q01`
        FROM `df`
      )
      WHERE (`q01` > 3.0) AND (`y` < 3.0)

# errors for named input

    Code
      filter(lf, x = 1)
    Condition
      Error in `check_filter()`:
      ! Problem with `filter()` input `..1`.
      x Input `..1` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

---

    Code
      filter(lf, y > 1, x = 1)
    Condition
      Error in `check_filter()`:
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

