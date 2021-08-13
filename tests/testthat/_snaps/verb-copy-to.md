# can translate a table

    Code
      db_values(con, df, name = "db_values_1") %>% remote_query()
    Output
      <SQL> SELECT NULL AS `dbl`, NULL AS `int`, NULL AS `chr`, NULL AS `dtt` WHERE false
      UNION ALL
      VALUES
        (1.5, 1, 'a', '2020-01-01T01:23:45Z')

# zero row table works

    Code
      db_values(con, tibble(dbl = numeric(), chr = character())) %>% remote_query()
    Output
      <SQL> SELECT NULL AS `dbl`, NULL AS `chr` WHERE false

# checks inputs

    Code
      expect_error(db_values(simulate_dbi(), tibble()))

---

    Code
      expect_error(db_values(simulate_dbi(), lazy_frame(a = 1)))

