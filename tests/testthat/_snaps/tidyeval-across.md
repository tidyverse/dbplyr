# across() gives meaningful messages

    Code
      (expect_error(lazy_frame(x = 1) %>% summarise(across(x, 42))))
    Output
      <error/rlang_error>
      Error in `across_funs()`:
      ! `.fns` argument to dbplyr::across() must be a NULL, a function, formula, or list
    Code
      (expect_error(lazy_frame(x = 1) %>% summarise(across(y, mean))))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `stop_subscript()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.

# across() translates functions

    Code
      lf %>% summarise(across(a:b, log))
    Output
      <SQL>
      SELECT LN(`a`) AS `a`, LN(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a:b, log, base = 2))
    Output
      <SQL>
      SELECT LOG(2.0, `a`) AS `a`, LOG(2.0, `b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a:b, list(log, exp)))
    Output
      <SQL>
      SELECT LN(`a`) AS `a_1`, EXP(`a`) AS `a_2`, LN(`b`) AS `b_1`, EXP(`b`) AS `b_2`
      FROM `df`

# untranslatable functions are preserved

    Code
      lf %>% summarise(across(a:b, SQL_LOG))
    Output
      <SQL>
      SELECT SQL_LOG(`a`) AS `a`, SQL_LOG(`b`) AS `b`
      FROM `df`

# across() translates formulas

    Code
      lf %>% summarise(across(a:b, ~ log(.x, 2)))
    Output
      <SQL>
      SELECT LOG(2.0, `a`) AS `a`, LOG(2.0, `b`) AS `b`
      FROM `df`

---

    Code
      lf %>% summarise(across(a:b, list(~ log(.x, 2))))
    Output
      <SQL>
      SELECT LOG(2.0, `a`) AS `a_1`, LOG(2.0, `b`) AS `b_1`
      FROM `df`

# across() translates NULL

    Code
      lf %>% mutate(across(a:b))
    Output
      <SQL>
      SELECT `a`, `b`
      FROM `df`

# old _at functions continue to work

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), "sum")
    Condition
      Warning:
      Missing values are always removed in SQL.
      Use `SUM(x, na.rm = TRUE)` to silence this warning
      This warning is displayed only once per session.
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), sum)
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

---

    Code
      lf %>% dplyr::summarise_at(dplyr::vars(a:b), ~ sum(.))
    Output
      <SQL>
      SELECT SUM(`a`) AS `a`, SUM(`b`) AS `b`
      FROM `df`

# across() defaults to everything()

    Code
      lazy_frame(x = 1, y = 1) %>% summarise(across(.fns = ~ . + 1))
    Output
      <SQL>
      SELECT `x` + 1.0 AS `x`, `y` + 1.0 AS `y`
      FROM `df`

# across() can use named selections

    Code
      df %>% summarise(across(c(a = x, b = y), list(mean = mean, sum = sum), na.rm = TRUE))
    Output
      <SQL>
      SELECT
        AVG(`x`) AS `a_mean`,
        SUM(`x`) AS `a_sum`,
        AVG(`y`) AS `b_mean`,
        SUM(`y`) AS `b_sum`
      FROM `df`

---

    Code
      df %>% summarise(across(all_of(c(a = "x", b = "y")), list(mean = mean, sum = sum)),
      na.rm = TRUE)
    Condition
      Warning:
      Missing values are always removed in SQL.
      Use `mean(x, na.rm = TRUE)` to silence this warning
      This warning is displayed only once per session.
    Output
      <SQL>
      SELECT
        AVG(`x`) AS `a_mean`,
        SUM(`x`) AS `a_sum`,
        AVG(`y`) AS `b_mean`,
        SUM(`y`) AS `b_sum`,
        TRUE AS `na.rm`
      FROM `df`

# if_all/any works in filter()

    Code
      lf %>% filter(if_all(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 AND `b` > 0.0)

---

    Code
      lf %>% filter(if_any(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 OR `b` > 0.0)

# if_all/any works in mutate()

    Code
      lf %>% mutate(c = if_all(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT `a`, `b`, `a` > 0.0 AND `b` > 0.0 AS `c`
      FROM `df`

---

    Code
      lf %>% mutate(c = if_any(a:b, ~ . > 0))
    Output
      <SQL>
      SELECT `a`, `b`, `a` > 0.0 OR `b` > 0.0 AS `c`
      FROM `df`

# if_all/any uses every colum as default

    Code
      lf %>% filter(if_all(.fns = ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 AND `b` > 0.0)

---

    Code
      lf %>% filter(if_any(.fns = ~ . > 0))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` > 0.0 OR `b` > 0.0)

# if_all/any works without `.fns` argument

    Code
      lf %>% filter(if_all(a:b))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` AND `b`)

---

    Code
      lf %>% filter(if_any(a:b))
    Output
      <SQL>
      SELECT *
      FROM `df`
      WHERE (`a` OR `b`)

