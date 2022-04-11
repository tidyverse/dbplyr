# frame is checked

    Code
      translate_sql(sum(x, na.rm = TRUE), vars_frame = c(1, 0))
    Condition
      Warning:
      Windowed expression 'SUM(`x`)' does not have explicit order.
      Please use arrange() or window_order() to make determinstic.
      Error in `rows()`:
      ! from must be less than to

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

# names windows automatically

    Code
      lf %>% mutate(across(c(col1), sum, na.rm = TRUE), across(c(col3), ~ order_by(
        desc(ord), cumsum(.x))))
    Output
      <SQL>
      SELECT
        SUM(`col1`) OVER (PARTITION BY `part`) AS `col1`,
        `col2`,
        SUM(`col3`) OVER (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING) AS `col3`,
        `col4`,
        `part`,
        `ord`
      FROM `df`
    Code
      lf %>% mutate(across(c(col1, col2), sum, na.rm = TRUE), across(c(col3, col4),
      ~ order_by(desc(ord), cumsum(.x))))
    Output
      <SQL>
      SELECT
        SUM(`col1`) OVER `win1` AS `col1`,
        SUM(`col2`) OVER `win1` AS `col2`,
        SUM(`col3`) OVER `win2` AS `col3`,
        SUM(`col4`) OVER `win2` AS `col4`,
        `part`,
        `ord`
      FROM `df`
      WINDOW
        `win1` AS (PARTITION BY `part`),
        `win2` AS (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING)

---

    Code
      lf %>% transmute(col1 = sum(col1, na.rm = TRUE), col3 = order_by(desc(ord),
      cumsum(col3)), col2 = sum(col2, na.rm = TRUE), col4 = order_by(desc(ord),
      cumsum(col4)))
    Output
      <SQL>
      SELECT
        `part`,
        SUM(`col1`) OVER `win1` AS `col1`,
        SUM(`col3`) OVER `win2` AS `col3`,
        SUM(`col2`) OVER `win1` AS `col2`,
        SUM(`col4`) OVER `win2` AS `col4`
      FROM `df`
      WINDOW
        `win1` AS (PARTITION BY `part`),
        `win2` AS (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING)

# name windows only if supported

    Code
      lf2 %>% mutate(across(c(col1, col2), sum, na.rm = TRUE), across(c(col3, col4),
      ~ order_by(desc(ord), cumsum(.x))))
    Output
      <SQL>
      SELECT
        SUM(`col1`) OVER (PARTITION BY `part`) AS `col1`,
        SUM(`col2`) OVER (PARTITION BY `part`) AS `col2`,
        SUM(`col3`) OVER (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING) AS `col3`,
        SUM(`col4`) OVER (PARTITION BY `part` ORDER BY `ord` DESC ROWS UNBOUNDED PRECEDING) AS `col4`,
        `part`,
        `ord`
      FROM `df`

