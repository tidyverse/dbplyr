# `rows_insert()` errors for `conflict = 'error'` and `in_place = FALSE`

    Code
      (expect_error(rows_insert(lf, lf, by = "x", conflict = "error", in_place = FALSE))
      )
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! `conflict = "error"` is not supported for `in_place = FALSE`.

# `rows_insert()` works with `in_place = FALSE`

    Code
      rows_insert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 3:4,
      y = 23:24, .name = "df_y"), by = "x", conflict = "ignore", in_place = FALSE)
    Output
      <SQL>
      (
        SELECT *
        FROM `df_x`
      )
      UNION ALL
      (
        SELECT * FROM `df_y` AS `LHS`
        WHERE NOT EXISTS (
          SELECT 1 FROM `df_x` AS `RHS`
          WHERE (`LHS`.`x` = `RHS`.`x`)
        )
      )

# `rows_insert()` works with `in_place = TRUE`

    `in_place = TRUE` does not work for simulated connections.

# `sql_query_insert()` works

    Code
      sql_query_insert(con = simulate_sqlite(), x_name = ident("df_x"), y = df_y, by = c(
        "a", "b"), conflict = "error", returning_cols = c("a", b2 = "b"))
    Output
      <SQL> INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) AS `...y`
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

---

    Code
      sql_query_insert(con = simulate_sqlite(), x_name = ident("df_x"), y = df_y, by = c(
        "a", "b"), conflict = "ignore", returning_cols = c("a", b2 = "b"))
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

# arguments are checked

    `by` must be a character vector.

---

    `by` must be unnamed.

---

    All columns specified through `by` must exist in `x` and `y`.
    i The following columns are missing from `x`: `z`.

---

    All columns in `y` must exist in `x`.
    i The following columns only exist in `y`: `z`.

---

    `returning` does not work for simulated connections.

---

    Can't determine name for target table. Set `in_place = FALSE` to return a lazy table.

# `rows_update()` works with `in_place = FALSE`

    Code
      rows_update(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      (
        SELECT * FROM `df_x` AS `LHS`
        WHERE NOT EXISTS (
          SELECT 1 FROM `df_y` AS `RHS`
          WHERE (`LHS`.`x` = `RHS`.`x`)
        )
      )
      UNION ALL
      (
        SELECT `LHS`.`x` AS `x`, `y`
        FROM (
          SELECT `x`
          FROM `df_x`
        ) `LHS`
        INNER JOIN `df_y` AS `RHS`
          ON (`LHS`.`x` = `RHS`.`x`)
      )

# `rows_update()` works with `in_place = TRUE`

    `in_place = TRUE` does not work for simulated connections.

# `sql_query_update_from()` works

    Code
      sql_query_update_from(con = simulate_mssql(), x_name = ident("df_x"), y = df_y,
      by = c("a", "b"), update_values = sql(c = "COALESCE(`df_x`.`c`, `...y`.`c`)",
        d = "`...y`.`d`"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> UPDATE `df_x`
      SET `c` = COALESCE(`df_x`.`c`, `...y`.`c`), `d` = `...y`.`d`
      OUTPUT `INSERTED`.`a`, `INSERTED`.`b` AS `b2`
      FROM `df_x`
      INNER JOIN (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        ON `...y`.`a` = `df_x`.`a` AND `...y`.`b` = `df_x`.`b`

# `rows_patch()` returns early if no column to update

    Code
      rows_patch(lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"), lazy_frame(
        x = 1:3, .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      SELECT *
      FROM `df_x`

# `rows_patch()` works with `in_place = FALSE`

    Code
      rows_patch(lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"), lazy_frame(
        x = 2:3, y = 22:23, .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      (
        SELECT * FROM `df_x` AS `LHS`
        WHERE NOT EXISTS (
          SELECT 1 FROM `df_y` AS `RHS`
          WHERE (`LHS`.`x` = `RHS`.`x`)
        )
      )
      UNION ALL
      (
        SELECT `x`, COALESCE(`y`, `y...y`) AS `y`
        FROM (
          SELECT `LHS`.`x` AS `x`, `LHS`.`y` AS `y`, `RHS`.`y` AS `y...y`
          FROM `df_x` AS `LHS`
          INNER JOIN `df_y` AS `RHS`
            ON (`LHS`.`x` = `RHS`.`x`)
        ) `q01`
      )

# `rows_patch()` works with `in_place = TRUE`

    `in_place = TRUE` does not work for simulated connections.

# `rows_upsert()` returns early if no column to update

    Code
      rows_upsert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 1:3,
      .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      (
        SELECT *
        FROM `df_x`
      )
      UNION ALL
      (
        SELECT `x`, NULL AS `y`
        FROM (
          SELECT * FROM `df_y` AS `LHS`
          WHERE NOT EXISTS (
            SELECT 1 FROM `df_x` AS `RHS`
            WHERE (`LHS`.`x` = `RHS`.`x`)
          )
        ) `q01`
      )

# `rows_upsert()` works with `in_place = FALSE`

    Code
      rows_upsert(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 2:3,
      y = 22:23, .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      (
        SELECT * FROM `df_x` AS `LHS`
        WHERE NOT EXISTS (
          SELECT 1 FROM `df_y` AS `RHS`
          WHERE (`LHS`.`x` = `RHS`.`x`)
        )
      )
      UNION ALL
      (
        (
          SELECT `LHS`.`x` AS `x`, `y`
          FROM (
            SELECT `x`
            FROM `df_x`
          ) `LHS`
          INNER JOIN `df_y` AS `RHS`
            ON (`LHS`.`x` = `RHS`.`x`)
        )
        UNION ALL
        (
          SELECT * FROM `df_y` AS `LHS`
          WHERE NOT EXISTS (
            SELECT 1 FROM `df_x` AS `RHS`
            WHERE (`LHS`.`x` = `RHS`.`x`)
          )
        )
      )

# `rows_upsert()` works with `in_place = TRUE`

    `in_place = TRUE` does not work for simulated connections.

# `sql_query_upsert()` is correct

    Code
      sql_query_upsert(con = simulate_dbi(), x_name = ident("df_x"), y = df_y, by = c(
        "a", "b"), update_cols = c("c", "d"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> WITH `updated` AS (
        UPDATE `df_x`
        SET `c` = `...y`.`c`, `d` = `...y`.`d`
        FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        WHERE (`...y`.`a` = `df_x`.`a`) AND (`...y`.`b` = `df_x`.`b`)
        RETURNING `df_x`.*
      )
      INSERT INTO `df_x` (`a`, `b`, `c`, `d`)
      SELECT *
      FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
      WHERE NOT EXISTS (
        SELECT 1 FROM `updated`
        WHERE (`updated`.`a` = `...y`.`a`) AND (`updated`.`b` = `...y`.`b`)
      )

# `rows_delete()` works with `in_place = FALSE`

    Code
      rows_delete(lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"), lazy_frame(
        x = 2:3, .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      SELECT * FROM `df_x` AS `LHS`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_y` AS `RHS`
        WHERE (`LHS`.`x` = `RHS`.`x`)
      )

# `rows_delete()` works with `in_place = TRUE`

    `in_place = TRUE` does not work for simulated connections.

# `sql_query_delete()` is correct

    Code
      sql_query_delete(con = simulate_dbi(), x_name = ident("df_x"), y = df_y, by = c(
        "a", "b"), returning_cols = c("a", b2 = "b"))
    Output
      <SQL> DELETE FROM `df_x`
      WHERE EXISTS (
        SELECT 1 FROM (
        SELECT `a`, `b`, `c` + 1.0 AS `c`, `d`
        FROM `df_y`
      ) `...y`
        WHERE (`...y`.`a` = `df_x`.`a`) AND (`...y`.`b` = `df_x`.`b`)
      )
      RETURNING `df_x`.`a`, `df_x`.`b` AS `b2`

# `get_returned_rows()` works

    No returned rows available.

