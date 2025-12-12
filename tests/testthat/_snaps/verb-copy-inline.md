# can translate a table

    Code
      remote_query(copy_inline(con, df))
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

# can translate blob columns

    Code
      show_query(db)
    Output
      <SQL>
      SELECT CAST(`x` AS BLOB) AS `x`
      FROM (
        SELECT NULL AS `x`
        WHERE (0 = 1)
      
        UNION ALL
      
        VALUES ((X'616263')), ((X'646566'))
      ) AS `values_table`

# can translate 1-column tables

    Code
      remote_query(copy_inline(con, tibble(dbl = 1.5)))
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
      remote_query(copy_inline(con, tibble(dbl = numeric(), chr = character())))
    Output
      <SQL> SELECT CAST(NULL AS REAL) AS `dbl`, CAST(NULL AS TEXT) AS `chr`
      WHERE (0 = 1)

---

    Code
      remote_query(copy_inline(con, tibble(dbl = numeric())))
    Output
      <SQL> SELECT CAST(NULL AS REAL) AS `dbl`
      WHERE (0 = 1)

# checks inputs

    Code
      (expect_error(copy_inline(con, tibble())))
    Output
      <error/rlang_error>
      Error in `copy_inline()`:
      ! `df` needs at least one column.
    Code
      (expect_error(copy_inline(con, lazy_frame(a = 1))))
    Output
      <error/rlang_error>
      Error in `copy_inline()`:
      ! `df` needs to be a data.frame.
    Code
      (expect_error(copy_inline(con, tibble(a = 1), types = c(b = "bigint"))))
    Output
      <error/rlang_error>
      Error in `copy_inline()`:
      ! Names of `df` and `types` must be the same.
    Code
      (expect_error(copy_inline(con, tibble(a = 1), types = c(b = 1))))
    Output
      <error/rlang_error>
      Error in `copy_inline()`:
      ! `types` must be a character vector, not the number 1.

