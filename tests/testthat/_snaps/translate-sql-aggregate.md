# aggregation functions warn once if na.rm = FALSE

    Code
      translate_sql(mean(x, na.rm = FALSE), con = con)
    Condition
      Warning:
      Missing values are always removed in SQL aggregation functions.
      Use `na.rm = TRUE` to silence this warning
      This warning is displayed once every 8 hours.
    Output
      <SQL> AVG("x") OVER ()

# warns informatively with unsupported function

    Code
      sql_not_supported("cor")()
    Condition
      Error:
      ! `cor()` is not available in this SQL variant.

# agg and win contexts suggest workarounds

    Code
      sql_not_supported("median", "agg", "SQL Server")(x)
    Condition
      Error:
      ! Translation of `median()` in `summarise()` is not supported for SQL Server.
      i Use a combination of `distinct()` and `mutate()` for the same result:
        `mutate(<col> = median(x)) |> distinct(<col>)`
    Code
      sql_not_supported("median", "agg")(x)
    Condition
      Error:
      ! Translation of `median()` in `summarise()` is not supported on database backends.
      i Use a combination of `distinct()` and `mutate()` for the same result:
        `mutate(<col> = median(x)) |> distinct(<col>)`
    Code
      sql_not_supported("median", "win", "Redshift")(x)
    Condition
      Error:
      ! Translation of `median()` in `mutate()` is not supported for Redshift.
      i Use a combination of `summarise()` and `left_join()` instead:
        `df |> left_join(summarise(<col> = median(x)))`.
    Code
      sql_not_supported("median", "win")(x)
    Condition
      Error:
      ! Translation of `median()` in `mutate()` is not supported on database backends.
      i Use a combination of `summarise()` and `left_join()` instead:
        `df |> left_join(summarise(<col> = median(x)))`.

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

