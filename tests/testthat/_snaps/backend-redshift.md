# copy_inline uses UNION ALL

    Code
      copy_inline(con, y %>% slice(0)) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS INTEGER) AS `id`, CAST(NULL AS TEXT) AS `arr`
      WHERE (0 = 1)
    Code
      copy_inline(con, y) %>% remote_query()
    Output
      <SQL> SELECT CAST(`id` AS INTEGER) AS `id`, CAST(`arr` AS TEXT) AS `arr`
      FROM (
        SELECT NULL AS `id`, NULL AS `arr`
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}'
      ) AS `values_table`
    Code
      copy_inline(con, y %>% slice(0), types = types) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS bigint) AS `id`, CAST(NULL AS integer[]) AS `arr`
      WHERE (0 = 1)
    Code
      copy_inline(con, y, types = types) %>% remote_query()
    Output
      <SQL> SELECT CAST(`id` AS bigint) AS `id`, CAST(`arr` AS integer[]) AS `arr`
      FROM (
        SELECT NULL AS `id`, NULL AS `arr`
        WHERE (0 = 1)
      
        UNION ALL
      
        SELECT 1, '{1,2,3}'
      ) AS `values_table`

# custom clock functions translated correctly

    Code
      test_translate_sql(date_count_between(date_column_1, date_column_2, "year"))
    Condition
      Error in `date_count_between()`:
      ! `precision = "year"` isn't supported on database backends.
      i It must be "day" instead.

---

    Code
      test_translate_sql(date_count_between(date_column_1, date_column_2, "day", n = 5))
    Condition
      Error in `date_count_between()`:
      ! `n = 5` isn't supported on database backends.
      i It must be 1 instead.

# difftime is translated correctly

    Code
      test_translate_sql(difftime(start_date, end_date, units = "auto"))
    Condition
      Error in `difftime()`:
      ! `units = "auto"` isn't supported on database backends.
      i It must be "days" instead.

---

    Code
      test_translate_sql(difftime(start_date, end_date, tz = "UTC", units = "days"))
    Condition
      Error in `difftime()`:
      ! Argument `tz` isn't supported on database backends.

