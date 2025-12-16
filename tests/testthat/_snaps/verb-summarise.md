# reframe is not supported

    Code
      reframe(lazy_frame(x = 1))
    Condition
      Error in `reframe()`:
      ! `reframe()` is not supported on database backends.

# can't refer to freshly created variables

    Code
      (expect_error(summarise(lf1, a_sum = sum(a), issue_col = sum(a_sum))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `issue_col = sum(a_sum)`
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
      i In argument: `issue_col = sum(a_sum)`
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
      i In argument: `issue_col = sum(b_sum)`
      Caused by error:
      ! In dbplyr you cannot use a variable created in the same `summarise()`.
      x `b_sum` was created earlier in this `summarise()`.
      i You need an extra `mutate()` step to use it.
    Code
      (expect_error(summarise(lf1, a_sum = sum(a), issue_col = across(a_sum, sum))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `issue_col = across(a_sum, sum)`
      Caused by error:
      ! In dbplyr you cannot use a variable created in the same `summarise()`.
      x `a_sum` was created earlier in this `summarise()`.
      i You need an extra `mutate()` step to use it.

# summarise(.groups=)

    Code
      eval_bare(expr(remote_query(dplyr::summarise(dplyr::group_by(lazy_frame(x = 1,
        y = 2), x, y)))), env(global_env()))
    Message
      `summarise()` has grouped output by "x". You can override using the `.groups` argument.
    Output
      <SQL> SELECT "x", "y"
      FROM "df"
      GROUP BY "x", "y"

---

    Code
      summarise(df, .groups = "rowwise")
    Condition
      Error in `summarise()`:
      ! `.groups` can't be "rowwise" in dbplyr
      i Possible values are NULL (default), "drop_last", "drop", and "keep"

# summarise can modify grouping variables

    Code
      (result1 <- summarise(group_by(lf, g), g = g + 1))
    Output
      <SQL>
      SELECT "g" + 1.0 AS "g"
      FROM "df"
      GROUP BY "g"

---

    Code
      (result2 <- summarise(group_by(lf, g), x = x + 1, g = g + 1))
    Output
      <SQL>
      SELECT "g" + 1.0 AS "g", "x" + 1.0 AS "x"
      FROM "df"
      GROUP BY "g"

# across() does not select grouping variables

    Code
      summarise(group_by(df, g), across(.fns = ~0))
    Output
      <SQL>
      SELECT "g", 0.0 AS "x"
      FROM "df"
      GROUP BY "g"

# across doesn't select columns from `.by` #1493

    Code
      out
    Output
      <SQL>
      SELECT "g", SUM("..x") AS "x"
      FROM "df"
      GROUP BY "g"

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
      sql_render(out)
    Output
      <SQL> SELECT `x`, COUNT(*) AS `n`
      FROM `verb-summarise`
      GROUP BY `x`
