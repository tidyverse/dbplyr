# can translate a table

    Code
      db_values(con, df) %>% remote_query()
    Output
      <SQL> SELECT CAST(`dbl` AS REAL) AS `dbl`, CAST(`int` AS INTEGER) AS `int`, `chr`, CAST(`dtt` AS TIMESTAMP) AS `dtt`
      FROM (SELECT NULL AS `dbl`, NULL AS `int`, NULL AS `chr`, NULL AS `dtt` WHERE 0 = 1
      UNION ALL
      VALUES
        (1.5, 1, 'a', '2020-01-01T01:23:45Z')) AS `values_table`

# can translate 1-column tables

    Code
      db_values(con, tibble(dbl = 1.5)) %>% remote_query()
    Output
      <SQL> SELECT CAST(`dbl` AS REAL) AS `dbl`
      FROM (SELECT NULL AS `dbl` WHERE 0 = 1
      UNION ALL
      VALUES
        (1.5)) AS `values_table`

# zero row table works

    Code
      db_values(con, tibble(dbl = numeric(), chr = character())) %>% remote_query()
    Output
      <SQL> SELECT CAST(`dbl` AS REAL) AS `dbl`, `chr`
      FROM (SELECT NULL AS `dbl`, NULL AS `chr` WHERE 0 = 1) AS `values_table`

---

    Code
      db_values(con, tibble(dbl = numeric())) %>% remote_query()
    Output
      <SQL> SELECT CAST(`dbl` AS REAL) AS `dbl`
      FROM (SELECT NULL AS `dbl` WHERE 0 = 1) AS `values_table`

# checks inputs

    Code
      expect_error(db_values(simulate_dbi(), tibble()))

---

    Code
      expect_error(db_values(simulate_dbi(), lazy_frame(a = 1)))

