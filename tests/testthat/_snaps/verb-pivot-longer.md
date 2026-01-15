# can pivot all cols to long

    Code
      tidyr::pivot_longer(lazy_frame(x = 1:2, y = 3:4), x:y)
    Output
      <SQL>
      SELECT 'x' AS "name", "x" AS "value"
      FROM "df"
      
      UNION ALL
      
      SELECT 'y' AS "name", "y" AS "value"
      FROM "df"

# can add multiple columns from spec

    Code
      pv
    Output
      <SQL>
      SELECT 11 AS "a", 13 AS "b", "x" AS "v"
      FROM "df"
      
      UNION ALL
      
      SELECT 12 AS "a", 14 AS "b", "y" AS "v"
      FROM "df"

# preserves original keys

    Code
      pv
    Output
      <SQL>
      SELECT "x", 'y' AS "name", "y" AS "value"
      FROM "df"
      
      UNION ALL
      
      SELECT "x", 'z' AS "name", "z" AS "value"
      FROM "df"

# can drop missing values

    Code
      tidyr::pivot_longer(lazy_frame(x = c(1, NA), y = c(NA, 2)), x:y,
      values_drop_na = TRUE)
    Output
      <SQL>
      SELECT *
      FROM (
        SELECT 'x' AS "name", "x" AS "value"
        FROM "df"
      
        UNION ALL
      
        SELECT 'y' AS "name", "y" AS "value"
        FROM "df"
      ) AS "q01"
      WHERE (NOT(("value" IS NULL)))

# can handle missing combinations

    Code
      show_query(sql)
    Output
      <SQL>
      SELECT *, NULL AS `y`
      FROM (
        SELECT `id`, '1' AS `n`, `x_1` AS `x`
        FROM `df`
      ) AS `q01`
      
      UNION ALL
      
      SELECT `id`, '2' AS `n`, `x_2` AS `x`, `y_2` AS `y`
      FROM `df`

# can override default output column type

    Code
      tidyr::pivot_longer(lazy_frame(x = 1), x, values_transform = list(value = as.character))
    Output
      <SQL>
      SELECT 'x' AS "name", CAST("x" AS TEXT) AS "value"
      FROM "df"

# values_transform can be a formula

    Code
      tidyr::pivot_longer(lazy_frame(x = 1), x, values_transform = list(value = ~
        as.character(.x)))
    Output
      <SQL>
      SELECT 'x' AS "name", CAST("x" AS TEXT) AS "value"
      FROM "df"

# `values_transform` is validated

    Code
      (expect_error(tidyr::pivot_longer(df, x, values_transform = 1)))
    Output
      <error/rlang_error>
      Error in `dbplyr_pivot_longer_spec()`:
      ! `values_transform` must be `NULL`, a function, or a named list of functions.
    Code
      (expect_error(tidyr::pivot_longer(df, x, values_transform = list(~.x))))
    Output
      <error/rlang_error>
      Error in `dbplyr_pivot_longer_spec()`:
      ! All elements of `values_transform` must be named.

# can pivot to multiple measure cols

    Code
      pv
    Output
      <SQL>
      SELECT 1.0 AS "row", "x" AS "X", "y" AS "Y"
      FROM "df"

# .value can be at any position in `names_to`

    Code
      value_first
    Output
      <SQL>
      SELECT "i", 't1' AS "time", "y_t1" AS "y", "z_t1" AS "z"
      FROM "df"
      
      UNION ALL
      
      SELECT "i", 't2' AS "time", "y_t2" AS "y", "z_t2" AS "z"
      FROM "df"

---

    Code
      value_second
    Output
      <SQL>
      SELECT "i", 't1' AS "time", "t1_y" AS "y", "t1_z" AS "z"
      FROM "df"
      
      UNION ALL
      
      SELECT "i", 't2' AS "time", "t2_y" AS "y", "t2_z" AS "z"
      FROM "df"

# can repair names

    Code
      out <- tidyr::pivot_longer(df, c(x, y), names_repair = "unique")
    Message
      New names:
      * `name` -> `name...2`
      * `value` -> `value...3`
      * `name` -> `name...4`
      * `value` -> `value...5`

# values_ptype is not supported

    Code
      tidyr::pivot_longer(lazy_frame(x = 1:2, y = 3:4), x:y, values_ptypes = character())
    Condition
      Error in `tidyr::pivot_longer()`:
      ! Argument `values_ptypes` isn't supported on database backends.

# cols_vary is not supported

    Code
      tidyr::pivot_longer(lazy_frame(x = 1:2, y = 3:4), x:y, cols_vary = "fastest")
    Condition
      Error in `tidyr::pivot_longer()`:
      ! Argument `cols_vary` isn't supported on database backends.

