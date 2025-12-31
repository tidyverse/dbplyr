# custom clock functions translated correctly

    Code
      translate_sql(date_count_between(x, y, "year"), con = con)
    Condition
      Error in `date_count_between()`:
      ! `precision = "year"` isn't supported on database backends.
      i It must be "day" instead.

---

    Code
      translate_sql(date_count_between(x, y, "day", n = 5), con = con)
    Condition
      Error in `date_count_between()`:
      ! `n = 5` isn't supported on database backends.
      i It must be 1 instead.

# difftime is translated correctly

    Code
      translate_sql(difftime(x, y, units = "auto"), con = con)
    Condition
      Error in `difftime()`:
      ! `units = "auto"` isn't supported on database backends.
      i It must be "days" instead.

---

    Code
      translate_sql(difftime(x, y, tz = "UTC"), con = con)
    Condition
      Error in `difftime()`:
      ! Argument `tz` isn't supported on database backends.

# first and last aggregate functions work

    Code
      translate_sql(nth(x, 2), "LAST(\"x\")", window = FALSE, con = con)
    Condition
      Error in `nth()`:
      ! `nth()` is only available in a windowed (`mutate()`) context

