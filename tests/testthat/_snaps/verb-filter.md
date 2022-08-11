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
        SELECT *, AVG(`x`) OVER () AS `q01`
        FROM `df`
      )
      WHERE (`q01` > 3.0) AND (`y` < 3.0)

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
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`, `h`
      HAVING (`g` = 1.0)

---

    Code
      (out <- lf %>% filter(x_mean > 1))
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`, `h`
      HAVING (AVG(`x`) > 1.0)

---

    Code
      (out <- lf %>% filter(g == 1) %>% filter(g == 2))
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`, `h`
      HAVING (`g` = 1.0) AND (`g` = 2.0)

---

    Code
      (out <- lf %>% filter(g == 1) %>% filter(h == 2))
    Output
      <SQL>
      SELECT `g`, `h`, AVG(`x`) AS `x_mean`
      FROM `df`
      GROUP BY `g`, `h`
      HAVING (`g` = 1.0) AND (`h` = 2.0)

# filter() after mutate() does not use `HAVING`

    Code
      (out <- lf %>% filter(x_mean > 1))
    Output
      <SQL>
      SELECT *
      FROM (
        SELECT *, AVG(`x`) OVER (PARTITION BY `g`, `h`) AS `x_mean`
        FROM `df`
      ) `q01`
      WHERE (`x_mean` > 1.0)

# filter() using a window function after summarise() does not use `HAVING`

    Code
      (out <- lf %>% filter(cumsum(x_mean) == 1))
    Condition
      Warning:
      Windowed expression `SUM(`x_mean`)` does not have explicit order.
      i Please use `arrange()` or `window_order()` to make deterministic.
    Output
      <SQL>
      SELECT `g`, `h`, `x_mean`
      FROM (
        SELECT
          *,
          SUM(`x_mean`) OVER (PARTITION BY `g` ROWS UNBOUNDED PRECEDING) AS `q02`
        FROM (
          SELECT `g`, `h`, AVG(`x`) AS `x_mean`
          FROM `df`
          GROUP BY `g`, `h`
        ) `q01`
      ) `q02`
      WHERE (`q02` = 1.0)

