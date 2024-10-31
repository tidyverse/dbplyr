# custom clock functions translated correctly

    Code
      test_translate_sql(date_count_between(date_column_1, date_column_2, "year"))
    Condition
      Error in `date_count_between()`:
      ! `precision` must be "day" on SQL backends.

---

    Code
      test_translate_sql(date_count_between(date_column_1, date_column_2, "day", n = 5))
    Condition
      Error in `date_count_between()`:
      ! `n` must be "1" on SQL backends.

# difftime is translated correctly

    Code
      test_translate_sql(difftime(start_date, end_date, units = "auto"))
    Condition
      Error in `difftime()`:
      ! The only supported value for `units` on SQL backends is "days"

---

    Code
      test_translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days"))
    Condition
      Error in `difftime()`:
      ! The `tz` argument is not supported for SQL backends.

