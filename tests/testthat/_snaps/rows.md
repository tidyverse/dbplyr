# rows_insert() checks arguments

    Code
      (rows_insert(lf, lf, by = "x", conflict = "error"))
    Condition
      Error in `rows_insert()`:
      ! `conflict = "error"` isn't supported on database backends.
      i It must be "ignore" instead.
    Code
      (rows_insert(lf, lf, by = "x"))
    Condition
      Error in `rows_insert()`:
      ! `conflict = "error"` isn't supported on database backends.
      i It must be "ignore" instead.

---

    Code
      (rows_insert(lf, lazy_frame(x = 1, y = 2, z = 3), by = "x", conflict = "ignore")
      )
    Condition
      Error in `rows_insert()`:
      ! All columns in `y` must exist in `x`.
      i The following columns only exist in `y`: `z`.

---

    Code
      (rows_insert(lf, lf, by = "x", conflict = "ignore", returning = everything()))
    Condition
      Error in `rows_insert()`:
      ! `returning` does not work for simulated connections.

---

    Code
      (df %>% mutate(x = x + 1) %>% rows_insert(df, by = "x", conflict = "ignore",
        in_place = TRUE))
    Condition
      Error in `target_table()`:
      ! Can't determine name for target table. Set `in_place = FALSE` to return a lazy table.

---

    Code
      (df %>% rows_insert(df, by = "x", conflict = "ignore", returning = c(y)))
    Condition
      Error in `rows_insert()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.

# `rows_insert()` errors for `conflict = 'error'` and `in_place = FALSE`

    Code
      (expect_error(rows_insert(lf, lf, by = "x", conflict = "error", in_place = FALSE))
      )
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! `conflict = "error"` isn't supported on database backends.
      i It must be "ignore" instead.

# `rows_insert()` works with `in_place = FALSE`

    Code
      rows_insert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 3:4,
      y = 23:24, .name = "df_y"), by = "x", conflict = "ignore", in_place = FALSE)
    Output
      <SQL>
      SELECT *
      FROM `df_x`
      
      UNION ALL
      
      SELECT `df_y`.*
      FROM `df_y`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_x`
        WHERE (`df_y`.`x` = `df_x`.`x`)
      )

# `rows_insert()` works with `in_place = TRUE`

    Code
      (rows_insert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", in_place = TRUE))
    Condition
      Error in `rows_insert()`:
      ! `in_place = TRUE` does not work for simulated connections.

# rows_get_or_execute() gives error context

    Code
      (expect_error(rows_append(tbl(con, "mtcars"), tibble(x = 1), copy = TRUE,
      in_place = TRUE)))
    Output
      <error/rlang_error>
      Error in `rows_append()`:
      ! Can't modify database table "mtcars".
      i Using SQL: INSERT INTO `mtcars` (`x`) SELECT * FROM ( SELECT * FROM `dbplyr_t2ne2IDfDW` ) AS `...y`
      Caused by error:
      ! dummy DBI error
    Code
      (expect_error(rows_append(tbl(con, "mtcars"), tibble(x = 1), copy = TRUE,
      in_place = TRUE, returning = x)))
    Output
      <error/rlang_error>
      Error in `rows_append()`:
      ! Can't modify database table "mtcars".
      i Using SQL: INSERT INTO `mtcars` (`x`) SELECT * FROM ( SELECT * FROM `dbplyr_QTqCFZRWtc` ) AS `...y` RETURNING `mtcars`.`x`
      Caused by error:
      ! dummy DBI error

# `sql_query_insert()` works

    Code
      (sql_query_insert(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), insert_cols = colnames(df_y), by = c("a", "b"), conflict = "error",
      returning_cols = c("a", b2 = "b")))
    Condition
      Error in `sql_query_insert()`:
      ! `conflict = "error"` isn't supported on database backends.
      i It must be "ignore" instead.

---

    Code
      sql_query_insert(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), insert_cols = colnames(df_y), by = c("a", "b"), conflict = "ignore",
      returning_cols = c("a", b2 = "b"))
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) AS `...y`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_x`
        WHERE (`df_x`.`a` = `...y`.`a`) AND (`df_x`.`b` = `...y`.`b`)
      )
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# rows_append() checks arguments

    Code
      (lf %>% rows_append(df, by = "x"))
    Condition
      Error in `rows_append()`:
      ! `...` must be empty.
      x Problematic argument:
      * by = "x"
    Code
      (lf %>% rows_append(df, conflict = "error"))
    Condition
      Error in `rows_append()`:
      ! `...` must be empty.
      x Problematic argument:
      * conflict = "error"

# `rows_append()` works with `in_place = FALSE`

    Code
      rows_append(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 3:4,
      y = 23:24, .name = "df_y"), in_place = FALSE)
    Output
      <SQL>
      SELECT *
      FROM `df_x`
      
      UNION ALL
      
      SELECT *
      FROM `df_y`

# `rows_append()` works with `in_place = TRUE`

    Code
      (rows_append(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), in_place = TRUE))
    Condition
      Error in `rows_append()`:
      ! `in_place = TRUE` does not work for simulated connections.

# `sql_query_append()` works

    Code
      sql_query_append(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), insert_cols = colnames(df_y), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) AS `...y`
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# sql_query_append supports old interface works

    Code
      sql_query_append(con = con, table = ident("df_x"), from = df_y, returning_cols = c(
        "a", b2 = "b"))
    Condition
      Warning:
      The `from` argument of `sql_query_append()` must be a table identifier or an SQL query, not a lazy table. as of dbplyr 2.3.2.
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) AS `...y`
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# arguments are checked

    Code
      (rows_update(lf, lf, by = 1, unmatched = "ignore"))
    Condition
      Error in `rows_update()`:
      ! `by` must be a character vector.

---

    Code
      (rows_update(lf, lf, by = c(y = "x"), unmatched = "ignore"))
    Condition
      Error in `rows_update()`:
      ! `by` must be unnamed.

---

    Code
      (rows_update(lf, lf, by = "z", unmatched = "ignore"))
    Condition
      Error in `rows_update()`:
      ! All columns specified through `by` must exist in `x` and `y`.
      i The following columns are missing from `x`: `z`.

---

    Code
      (rows_update(lf, lf, by = "x", unmatched = "error"))
    Condition
      Error in `rows_update()`:
      ! `unmatched = "error"` isn't supported on database backends.
      i It must be "ignore" instead.
    Code
      (rows_update(lf, lf, by = "x"))
    Condition
      Error in `rows_update()`:
      ! `unmatched = "error"` isn't supported on database backends.
      i It must be "ignore" instead.

---

    Code
      (rows_update(lf, lazy_frame(x = 1, y = 2, z = 3), by = "x", unmatched = "ignore")
      )
    Condition
      Error in `rows_update()`:
      ! All columns in `y` must exist in `x`.
      i The following columns only exist in `y`: `z`.

---

    Code
      (rows_update(lf, lf, by = "x", unmatched = "ignore", returning = everything()))
    Condition
      Error in `rows_update()`:
      ! `returning` does not work for simulated connections.

---

    Code
      (df %>% mutate(x = x + 1) %>% rows_update(df, by = "x", unmatched = "ignore",
        in_place = TRUE))
    Condition
      Error in `target_table()`:
      ! Can't determine name for target table. Set `in_place = FALSE` to return a lazy table.

# `rows_update()` works with `in_place = FALSE`

    Code
      rows_update(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", unmatched = "ignore", in_place = FALSE)
    Output
      <SQL>
      SELECT `df_x`.*
      FROM `df_x`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_y`
        WHERE (`df_x`.`x` = `df_y`.`x`)
      )
      
      UNION ALL
      
      SELECT `df_x`.`x` AS `x`, `df_y`.`y` AS `y`
      FROM `df_x`
      INNER JOIN `df_y`
        ON (`df_x`.`x` = `df_y`.`x`)

# `rows_update()` works with `in_place = TRUE`

    Code
      (rows_update(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", unmatched = "ignore", in_place = TRUE))
    Condition
      Error in `rows_update()`:
      ! `in_place = TRUE` does not work for simulated connections.

# `sql_query_update_from()` works

    Code
      sql_query_update_from(con = con, table = ident("df_x"), from = sql_render(df_y,
        con, lvl = 1), by = c("a", "b"), update_values = sql(c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> UPDATE `df_x`
      SET `c` = COALESCE(`df_x`.`c`, `...y`.`c`), `d` = `...y`.`d`
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) AS `...y`
      WHERE (`...y`.`a` = `df_x`.`a`) AND (`...y`.`b` = `df_x`.`b`)
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# `rows_patch()` returns early if no column to update

    Code
      rows_patch(lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"), lazy_frame(
        x = 1:3, .name = "df_y"), by = "x", unmatched = "ignore", in_place = FALSE)
    Output
      <SQL>
      SELECT *
      FROM `df_x`

# `rows_patch()` works with `in_place = FALSE`

    Code
      rows_patch(lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"), lazy_frame(
        x = 2:3, y = 22:23, .name = "df_y"), by = "x", unmatched = "ignore",
      in_place = FALSE)
    Output
      <SQL>
      SELECT `df_x`.*
      FROM `df_x`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_y`
        WHERE (`df_x`.`x` = `df_y`.`x`)
      )
      
      UNION ALL
      
      SELECT `x`, COALESCE(`y`, `y...y`) AS `y`
      FROM (
        SELECT `df_x`.*, `df_y`.`y` AS `y...y`
        FROM `df_x`
        INNER JOIN `df_y`
          ON (`df_x`.`x` = `df_y`.`x`)
      ) AS `q01`

# `rows_patch()` works with `in_place = TRUE`

    Code
      (rows_patch(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", unmatched = "ignore", in_place = TRUE))
    Condition
      Error in `rows_patch()`:
      ! `in_place = TRUE` does not work for simulated connections.

# `rows_upsert()` returns early if no column to update

    Code
      rows_upsert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 1:3,
      .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      SELECT *
      FROM `df_x`
      
      UNION ALL
      
      SELECT `q01`.*, NULL AS `y`
      FROM (
        SELECT `df_y`.*
        FROM `df_y`
        WHERE NOT EXISTS (
          SELECT 1 FROM `df_x`
          WHERE (`df_y`.`x` = `df_x`.`x`)
        )
      ) AS `q01`

# `rows_upsert()` works with `in_place = FALSE`

    Code
      rows_upsert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      SELECT `df_x`.*
      FROM `df_x`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_y`
        WHERE (`df_x`.`x` = `df_y`.`x`)
      )
      
      UNION ALL
      
      SELECT `df_x`.`x` AS `x`, `df_y`.`y` AS `y`
      FROM `df_x`
      INNER JOIN `df_y`
        ON (`df_x`.`x` = `df_y`.`x`)
      
      UNION ALL
      
      SELECT `df_y`.*
      FROM `df_y`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_x`
        WHERE (`df_y`.`x` = `df_x`.`x`)
      )

# `rows_upsert()` works with `in_place = TRUE`

    Code
      (rows_upsert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", in_place = TRUE))
    Condition
      Error in `rows_upsert()`:
      ! `in_place = TRUE` does not work for simulated connections.

# `sql_query_upsert()` is correct

    Code
      sql_query_upsert(con = con, table = ident("df_x"), from = sql_render(df_y, con,
        lvl = 1), by = c("a", "b"), update_cols = c("c", "d"), returning_cols = c("a",
        b2 = "b"))
    Output
      <SQL> WITH `updated` AS (
        UPDATE `df_x`
        SET `c` = `...y`.`c`, `d` = `...y`.`d`
        FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) AS `...y`
        WHERE (`...y`.`a` = `df_x`.`a`) AND (`...y`.`b` = `df_x`.`b`)
        RETURNING `df_x`.*
      )
      INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) AS `...y`
      WHERE NOT EXISTS (
        SELECT 1 FROM `updated`
        WHERE (`updated`.`a` = `...y`.`a`) AND (`updated`.`b` = `...y`.`b`)
      )
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# `rows_delete()` works with `in_place = FALSE`

    Code
      rows_delete(lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"), lazy_frame(
        x = 2:3, .name = "df_y"), by = "x", unmatched = "ignore", in_place = FALSE)
    Output
      <SQL>
      SELECT `df_x`.*
      FROM `df_x`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_y`
        WHERE (`df_x`.`x` = `df_y`.`x`)
      )

# `rows_delete()` works with `in_place = TRUE`

    Code
      (rows_delete(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      .name = "df_y"), by = "x", unmatched = "ignore", in_place = TRUE))
    Condition
      Error in `rows_delete()`:
      ! `in_place = TRUE` does not work for simulated connections.

# `sql_query_delete()` is correct

    Code
      sql_query_delete(con = simulate_dbi(), table = ident("df_x"), from = sql_render(
        df_y, simulate_dbi(), lvl = 2), by = c("a", "b"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> DELETE FROM `df_x`
      WHERE EXISTS (
        SELECT 1 FROM (
          SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
          FROM `df_y`
        ) AS `...y`
        WHERE (`...y`.`a` = `df_x`.`a`) AND (`...y`.`b` = `df_x`.`b`)
      )
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# `get_returned_rows()` works

    Code
      (get_returned_rows(df))
    Condition
      Error in `get_returned_rows()`:
      ! No returned rows available.

