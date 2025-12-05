# custom clock functions translated correctly

    Code
      translate_sql(date_count_between(date_column_1, date_column_2, "year"), con = con)
    Condition
      Error in `date_count_between()`:
      ! `precision = "year"` isn't supported on database backends.
      i It must be "day" instead.

---

    Code
      translate_sql(date_count_between(date_column_1, date_column_2, "day", n = 5),
      con = con)
    Condition
      Error in `date_count_between()`:
      ! `n = 5` isn't supported on database backends.
      i It must be 1 instead.

# difftime is translated correctly

    Code
      translate_sql(difftime(start_date, end_date, units = "auto"), con = con)
    Condition
      Error in `difftime()`:
      ! `units = "auto"` isn't supported on database backends.
      i It must be "days" instead.

---

    Code
      translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days"), con = con)
    Condition
      Error in `difftime()`:
      ! Argument `tz` isn't supported on database backends.

