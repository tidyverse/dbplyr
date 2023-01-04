# can refer to fresly created values

    Code
      show_query(out2)
    Output
      <SQL>
      SELECT `x` + 4.0 AS `x`
      FROM (
        SELECT `x` + 2.0 AS `x`
        FROM (
          SELECT `x` + 1.0 AS `x`
          FROM `multi_mutate`
        )
      )

# transmute includes all needed variables

    Code
      out
    Output
      <SQL>
      SELECT `x`, `x` + `y` AS `x2`
      FROM (
        SELECT `x` / 2.0 AS `x`, `y`
        FROM `df`
      ) `q01`

# across() does not select grouping variables

    Code
      df %>% group_by(g) %>% mutate(across(.fns = ~0))
    Output
      <SQL>
      SELECT `g`, 0.0 AS `x`
      FROM `df`

---

    Code
      df %>% group_by(g) %>% transmute(across(.fns = ~0))
    Output
      <SQL>
      SELECT `g`, 0.0 AS `x`
      FROM `df`

# across() can access previously created variables

    Code
      remote_query(lf)
    Output
      <SQL> SELECT `x`, SQRT(`y`) AS `y`
      FROM (
        SELECT *, 2.0 AS `y`
        FROM `df`
      ) `q01`

# new columns take precedence over global variables

    Code
      remote_query(lf)
    Output
      <SQL> SELECT *, `y` + 1.0 AS `z`
      FROM (
        SELECT *, 2.0 AS `y`
        FROM `df`
      ) `q01`

# mutate() produces nice error messages

    Code
      lazy_frame(x = 1) %>% mutate(z = non_existent + 1)
    Condition
      Error in `mutate()`:
      i In argument: `z = non_existent + 1`
      Caused by error:
      ! Object `non_existent` not found.
    Code
      lazy_frame(x = 1) %>% mutate(across(x, mean, na.rm = z))
    Condition
      Error in `mutate()`:
      i In argument: `across(x, mean, na.rm = z)`
      Caused by error in `across()`:
      ! Problem while evaluating `na.rm = z`.
      Caused by error:
      ! Object `z` not found.
    Code
      lazy_frame(x = 1) %>% mutate(across(x, .fns = "a"))
    Condition
      Error in `mutate()`:
      i In argument: `across(x, .fns = "a")`
      Caused by error in `across()`:
      ! `.fns` must be a function, a formula, or list of functions/formulas.

# mutate generates subqueries as needed

    Code
      lf %>% mutate(x = x + 1, x = x + 1)
    Output
      <SQL>
      SELECT `x` + 1.0 AS `x`
      FROM (
        SELECT `x` + 1.0 AS `x`
        FROM `df`
      )

---

    Code
      lf %>% mutate(x1 = x + 1, x2 = x1 + 1)
    Output
      <SQL>
      SELECT *, `x1` + 1.0 AS `x2`
      FROM (
        SELECT *, `x` + 1.0 AS `x1`
        FROM `df`
      )

# mutate collapses over nested select

    Code
      lf %>% select(x:y) %>% mutate(x = x * 2, y = y * 2)
    Output
      <SQL>
      SELECT `x` * 2.0 AS `x`, `y` * 2.0 AS `y`
      FROM `df`

---

    Code
      lf %>% select(y:x) %>% mutate(x = x * 2, y = y * 2)
    Output
      <SQL>
      SELECT `y` * 2.0 AS `y`, `x` * 2.0 AS `x`
      FROM `df`

# var = NULL works when var is in original data

    Code
      remote_query(lf)
    Output
      <SQL> SELECT `x` * 2.0 AS `z`
      FROM (
        SELECT 2.0 AS `x`
        FROM `df`
      ) `q01`

# var = NULL when var is in final output

    Code
      remote_query(lf)
    Output
      <SQL> SELECT *, 3.0 AS `y`
      FROM `df`

# temp var with nested arguments

    Code
      remote_query(lf)
    Output
      <SQL> SELECT `x`, `y` * 2.0 AS `z`
      FROM (
        SELECT *, 2.0 AS `y`
        FROM `df`
      ) `q01`

