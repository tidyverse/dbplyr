# df must be a local or remote table

    Code
      copy_to(con, list(x = 1), name = "df")
    Condition
      Error in `copy_to()`:
      ! `df` must be a local dataframe or a remote tbl_sql

# can translate a table

    Code
      copy_inline(con, df) %>% remote_query()
    Output
      <SQL> SELECT
        CAST(`lgl` AS BOOLEAN) AS `lgl`,
        CAST(`int` AS INTEGER) AS `int`,
        CAST(`dbl` AS REAL) AS `dbl`,
        CAST(`chr` AS TEXT) AS `chr`,
        CAST(`date` AS TEXT) AS `date`,
        CAST(`dtt` AS TEXT) AS `dtt`
      FROM (
        SELECT
          NULL AS `lgl`,
          NULL AS `int`,
          NULL AS `dbl`,
          NULL AS `chr`,
          NULL AS `date`,
          NULL AS `dtt`
        WHERE (0 = 1)
        UNION ALL
        VALUES (1, 1, 1.5, 'a', '2020-01-01', '2020-01-01T01:23:45Z')
      ) AS `values_table`

# can translate 1-column tables

    Code
      copy_inline(con, tibble(dbl = 1.5)) %>% remote_query()
    Output
      <SQL> SELECT CAST(`dbl` AS REAL) AS `dbl`
      FROM (
        SELECT NULL AS `dbl`
        WHERE (0 = 1)
        UNION ALL
        VALUES (1.5)
      ) AS `values_table`

# zero row table works

    Code
      copy_inline(con, tibble(dbl = numeric(), chr = character())) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS REAL) AS `dbl`, CAST(NULL AS TEXT) AS `chr`
      WHERE (0 = 1)

---

    Code
      copy_inline(con, tibble(dbl = numeric())) %>% remote_query()
    Output
      <SQL> SELECT CAST(NULL AS REAL) AS `dbl`
      WHERE (0 = 1)

# checks inputs

    Code
      (expect_error(copy_inline(simulate_dbi(), tibble())))
    Output
      <error/rlang_error>
      Error in `copy_inline()`:
      ! `df` needs at least one column.
    Code
      (expect_error(copy_inline(simulate_dbi(), lazy_frame(a = 1))))
    Output
      <error/rlang_error>
      Error in `copy_inline()`:
      ! `df` needs to be a data.frame.

