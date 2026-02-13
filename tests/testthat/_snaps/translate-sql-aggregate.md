# warns informatively with unsupported function

    Code
      sql_not_supported("cor")()
    Condition
      Error:
      ! `cor()` is not available in this SQL variant.

# quantile and median don't change without warning

    Code
      translate_sql(quantile(x, 0.75, na.rm = TRUE), con = con, window = FALSE)
    Output
      <SQL> PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY "x")

---

    Code
      translate_sql(quantile(x, 0.75, na.rm = TRUE), con = con, vars_group = "g")
    Output
      <SQL> PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY "x") OVER (PARTITION BY "g")

---

    Code
      translate_sql(median(x, na.rm = TRUE), con = con, window = FALSE)
    Output
      <SQL> PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY "x")

---

    Code
      translate_sql(median(x, na.rm = TRUE), con = con, vars_group = "g")
    Output
      <SQL> PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY "x") OVER (PARTITION BY "g")

