# frame is checked

    Code
      translate_sql(sum(x, na.rm = TRUE), vars_frame = c(1, 0))
    Condition
      Warning:
      Windowed expression `SUM(`x`)` does not have explicit order.
      Please use `arrange()` or `window_order()` to make determinstic.
      Error in `rows()`:
      ! `from` must be less than `to`

# window_frame()

    Code
      lf %>% window_frame(-3, 0) %>% window_order(x) %>% mutate(z = sum(y)) %>%
        show_query()
    Condition
      Warning:
      Missing values are always removed in SQL.
      Use `SUM(x, na.rm = TRUE)` to silence this warning
      This warning is displayed only once per session.
    Output
      <SQL>
      SELECT `x`, `y`, SUM(`y`) OVER (ORDER BY `x` ROWS 3 PRECEDING) AS `z`
      FROM `df`

# window_frame() checks arguments

    Code
      window_frame(lf, "a")
    Condition
      Error in `window_frame()`:
      ! is.numeric(from) is not TRUE

---

    Code
      window_frame(lf, 1:2)
    Condition
      Error in `window_frame()`:
      ! length(from) == 1 is not TRUE

---

    Code
      window_frame(lf, 1, "a")
    Condition
      Error in `window_frame()`:
      ! is.numeric(to) is not TRUE

---

    Code
      window_frame(lf, 1, 1:2)
    Condition
      Error in `window_frame()`:
      ! length(to) == 1 is not TRUE

