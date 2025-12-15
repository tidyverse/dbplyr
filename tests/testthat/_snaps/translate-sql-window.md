# frame is checked

    Code
      translate_sql(sum(x, na.rm = TRUE), con = con, vars_frame = c(1, 0))
    Condition
      Warning:
      Windowed expression `SUM(`x`)` does not have explicit order.
      i Please use `arrange()` or `window_order()` to make deterministic.
      Error in `rows()`:
      ! `from` (1) must be less than `to` (0)

# win_rank(c()) gives an informative error

    Code
      translate_sql(row_number(c(x)), con = con)
    Condition
      Error in `row_number()`:
      ! Can't use `c()` in `ROW_NUMBER()`
      i Did you mean to use `tibble(x)` instead?

# row_number() with and without group_by() and arrange()

    Code
      mutate(mf, rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER () AS `rown`
      FROM `df`

---

    Code
      mutate(group_by(mf, y), rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER (PARTITION BY `y`) AS `rown`
      FROM `df`

---

    Code
      mutate(arrange(group_by(mf, y), y), rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER (PARTITION BY `y` ORDER BY `y`) AS `rown`
      FROM `df`
      ORDER BY `y`

---

    Code
      mutate(arrange(mf, y), rown = row_number())
    Output
      <SQL>
      SELECT `df`.*, ROW_NUMBER() OVER (ORDER BY `y`) AS `rown`
      FROM `df`
      ORDER BY `y`

# window_frame()

    Code
      show_query(mutate(window_order(window_frame(lf, -3, 0), x), z = sum(y, na.rm = TRUE)))
    Output
      <SQL>
      SELECT `df`.*, SUM(`y`) OVER (ORDER BY `x` ROWS 3 PRECEDING) AS `z`
      FROM `df`

---

    Code
      show_query(mutate(window_order(window_frame(lf, -3), x), z = sum(y, na.rm = TRUE)))
    Output
      <SQL>
      SELECT
        `df`.*,
        SUM(`y`) OVER (ORDER BY `x` ROWS BETWEEN 3 PRECEDING AND UNBOUNDED FOLLOWING) AS `z`
      FROM `df`

# window_frame() checks arguments

    Code
      window_frame(lf, "a")
    Condition
      Error in `window_frame()`:
      ! `from` must be a whole number, not the string "a".

---

    Code
      window_frame(lf, 1:2)
    Condition
      Error in `window_frame()`:
      ! `from` must be a whole number, not an integer vector.

---

    Code
      window_frame(lf, 1, "a")
    Condition
      Error in `window_frame()`:
      ! `to` must be a whole number, not the string "a".

---

    Code
      window_frame(lf, 1, 1:2)
    Condition
      Error in `window_frame()`:
      ! `to` must be a whole number, not an integer vector.

