# can't refer to freshly created variables

    Code
      summarise(mf1, y = sum(x), z = sum(y))
    Condition
      Error in `summarise()`:
      ! In dbplyr you cannot use a variable created in the same `summarise()`.
      x `z` refers to `y` which was created earlier in this `summarise()`.
      i You need an extra `mutate()` step to use it.

# summarise(.groups=)

    Code
      eval_bare(expr(lazy_frame(x = 1, y = 2) %>% dplyr::group_by(x, y) %>% dplyr::summarise() %>%
        remote_query()), env(global_env()))
    Message
      `summarise()` has grouped output by "x". You can override using the `.groups` argument.
    Output
      <SQL> SELECT `x`, `y`
      FROM `df`
      GROUP BY `x`, `y`

---

    Code
      df %>% summarise(.groups = "rowwise")
    Condition
      Error in `summarise()`:
      ! `.groups` can't be "rowwise" in dbplyr
      i Possible values are NULL (default), "drop_last", "drop", and "keep"

# summarise can modify grouping variables

    Code
      (result1 <- lf %>% group_by(g) %>% summarise(g = g + 1))
    Output
      <SQL>
      SELECT `g` + 1.0 AS `g`
      FROM `df`
      GROUP BY `g`

---

    Code
      (result2 <- lf %>% group_by(g) %>% summarise(x = x + 1, g = g + 1))
    Output
      <SQL>
      SELECT `g` + 1.0 AS `g`, `x` + 1.0 AS `x`
      FROM `df`
      GROUP BY `g`

# across() does not select grouping variables

    Code
      df %>% group_by(g) %>% summarise(across(.fns = ~0))
    Output
      <SQL>
      SELECT `g`, 0.0 AS `x`
      FROM `df`
      GROUP BY `g`

# quoting for rendering summarized grouped table

    Code
      out %>% sql_render
    Output
      <SQL> SELECT `x`, COUNT(*) AS `n`
      FROM `verb-summarise`
      GROUP BY `x`

