# quantile and median don't change without warning

    Code
      test_translate_sql(quantile(x, 0.75, na.rm = TRUE), window = FALSE)
    Output
      <SQL> PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY `x`)

---

    Code
      test_translate_sql(quantile(x, 0.75, na.rm = TRUE), vars_group = "g")
    Output
      <SQL> PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY `x`) OVER (PARTITION BY `g`)

---

    Code
      test_translate_sql(median(x, na.rm = TRUE), window = FALSE)
    Output
      <SQL> PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY `x`)

---

    Code
      test_translate_sql(median(x, na.rm = TRUE), vars_group = "g")
    Output
      <SQL> PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY `x`) OVER (PARTITION BY `g`)

