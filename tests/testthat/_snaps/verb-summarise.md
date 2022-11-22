# can't refer to freshly created variables

    Code
      (expect_error(summarise(lf1, a_sum = sum(a), issue_col = sum(a_sum))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `issue_col = sum(a_sum)`
      Caused by error:
      ! In dbplyr you cannot use a variable created in the same `summarise()`.
      x `a_sum` was created earlier in this `summarise()`.
      i You need an extra `mutate()` step to use it.
    Code
      (expect_error(summarise(lf1, across(c(a, b), list(sum = sum)), issue_col = sum(
        a_sum))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `issue_col = sum(a_sum)`
      Caused by error:
      ! In dbplyr you cannot use a variable created in the same `summarise()`.
      x `a_sum` was created earlier in this `summarise()`.
      i You need an extra `mutate()` step to use it.
    Code
      (expect_error(summarise(lf1, across(c(a, b), list(sum = sum)), issue_col = sum(
        b_sum))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `issue_col = sum(b_sum)`
      Caused by error:
      ! In dbplyr you cannot use a variable created in the same `summarise()`.
      x `b_sum` was created earlier in this `summarise()`.
      i You need an extra `mutate()` step to use it.
    Code
      (expect_error(summarise(lf1, a_sum = sum(a), issue_col = across(a_sum, sum))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `issue_col = across(a_sum, sum)`
      Caused by error:
      ! In dbplyr you cannot use a variable created in the same `summarise()`.
      x `a_sum` was created earlier in this `summarise()`.
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

# can't use `.by` with `.groups`

    Code
      summarise(df, .by = x, .groups = "drop")
    Condition
      Error in `summarise()`:
      ! Can't supply both `.by` and `.groups`.

# catches `.by` with grouped-df

    Code
      summarise(gdf, .by = x)
    Condition
      Error:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# quoting for rendering summarized grouped table

    Code
      out %>% sql_render
    Output
      <SQL> SELECT `x`, COUNT(*) AS `n`
      FROM `verb-summarise`
      GROUP BY `x`

