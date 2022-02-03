# can pivot all cols to wide

    Code
      lazy_frame(key = c("x", "y", "z"), val = 1:3) %>% dbplyr_pivot_wider_spec(spec)
    Output
      <SQL>
      SELECT
        MAX(CASE WHEN (`key` = 'x') THEN `val` END) AS `x`,
        MAX(CASE WHEN (`key` = 'y') THEN `val` END) AS `y`,
        MAX(CASE WHEN (`key` = 'z') THEN `val` END) AS `z`
      FROM `df`

# implicit missings turn into explicit missings

    Code
      lazy_frame(a = 1:2, key = c("x", "y"), val = 1:2) %>% dbplyr_pivot_wider_spec(
        spec)
    Output
      <SQL>
      SELECT
        `a`,
        MAX(CASE WHEN (`key` = 'x') THEN `val` END) AS `x`,
        MAX(CASE WHEN (`key` = 'y') THEN `val` END) AS `y`
      FROM `df`
      GROUP BY `a`

# error when overwriting existing column

    Code
      tidyr::pivot_wider(df, names_from = key, values_from = val)
    Condition
      Error in `stop_vctrs()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.

# values_fn can be a single function

    Code
      dbplyr_pivot_wider_spec(df, spec1, values_fn = sum)
    Output
      <SQL>
      SELECT `a`, SUM(CASE WHEN (`key` = 'x') THEN `val` END) AS `x`
      FROM `df`
      GROUP BY `a`

# values_fn can be a formula

    Code
      dbplyr_pivot_wider_spec(df, spec1, values_fn = ~ sum(.x, na.rm = TRUE))
    Output
      <SQL>
      SELECT `a`, SUM(CASE WHEN (`key` = 'x') THEN `val` END) AS `x`
      FROM `df`
      GROUP BY `a`

# values_fn can be a named list

    `values_fn` must specify a function for each col in `values_from`

---

    `values_fn` must specify a function for each col in `values_from`

# values_fn cannot be NULL

    Code
      dbplyr_pivot_wider_spec(df, spec1, values_fn = NULL)
    Condition
      Error in `dbplyr_pivot_wider_spec()`:
      ! `values_fn` must not be NULL
      i `values_fn` must be a function or a named list of functions

# can fill in missing cells

    Code
      dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = 0)
    Output
      <SQL>
      SELECT
        `g`,
        `name`,
        `value`,
        MAX(CASE WHEN (`key` = 'x') THEN `val` WHEN NOT (`key` = 'x') THEN 0.0 END) AS `x`,
        MAX(CASE WHEN (`key` = 'y') THEN `val` WHEN NOT (`key` = 'y') THEN 0.0 END) AS `y`
      FROM `df`
      GROUP BY `g`, `name`, `value`

---

    Code
      dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = list(value = 0))
    Output
      <SQL>
      SELECT
        `g`,
        `name`,
        `value`,
        MAX(CASE WHEN (`key` = 'x') THEN `val` END) AS `x`,
        MAX(CASE WHEN (`key` = 'y') THEN `val` END) AS `y`
      FROM `df`
      GROUP BY `g`, `name`, `value`

# values_fill is checked

    Code
      dbplyr_pivot_wider_spec(lf, spec, values_fill = 1:2)
    Condition
      Error in `dbplyr_pivot_wider_spec()`:
      ! `values_fill` must be NULL, a scalar, or a named list

# cannot pivot lazy frames

    Code
      tidyr::pivot_wider(lazy_frame(name = "x", value = 1))
    Condition
      Error in `dbplyr_build_wider_spec()`:
      ! `dbplyr_build_wider_spec()` doesn't work with local lazy tibbles.
      i Use `memdb_frame()` together with `show_query()` to see the SQL code.

