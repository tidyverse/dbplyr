# arguments are checked

    `by` must be a character vector.

---

    `by` must be unnamed.

---

    All `by` columns must exist in `x`.

---

    All columns in `y` must exist in `x`.

---

    `returning` does not work for simulated connections.

---

    Can't determine name for target table. Set `in_place = FALSE` to return a lazy table.

# `rows_update()` returns early if no column to update

    Code
      rows_update(lazy_frame(x = 1:3, y = 11:13, .name = "df_x"), lazy_frame(x = 1:3,
      .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      SELECT *
      FROM `df_x`

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

# `rows_delete()` works with `in_place = FALSE`

    Code
      rows_delete(lazy_frame(x = 1:3, y = c(11, 12, NA), .name = "df_x"), lazy_frame(
        x = 2:3, y = 22:23, .name = "df_y"), by = "x", in_place = FALSE)
    Output
      <SQL>
      SELECT * FROM `df_x` AS `LHS`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df_y` AS `RHS`
        WHERE (`LHS`.`x` = `RHS`.`x`)
      )

# `rows_delete()` works with `in_place = TRUE`

    `in_place = TRUE` does not work for simulated connections.

# `get_returned_rows()` works

    No returned rows available.

