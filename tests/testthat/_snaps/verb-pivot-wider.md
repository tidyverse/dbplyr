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
      Error in `dbplyr_pivot_wider_spec()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.

# values_fn can be a single function

    Code
      suppressWarnings(dbplyr_pivot_wider_spec(df, spec1, values_fn = sum))
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

    Can't pivot column b:
    Caused by error:
    ! Can't convert `values_fn$b`, `NULL`, to a function.

# values_fn cannot be NULL

    Code
      dbplyr_pivot_wider_spec(df, spec1, values_fn = NULL)
    Condition
      Error in `dbplyr_pivot_wider_spec()`:
      ! `values_fn` must specify a function for each col in `values_from`

# `unused_fn` is validated

    Code
      (expect_error(tidyr::pivot_wider(df, id_cols = id, unused_fn = 1)))
    Output
      <error/rlang_error>
      Error in `tidyr::pivot_wider()`:
      ! `unused_fn` must be `NULL`, a function, or a named list of functions.

# can fill in missing cells

    Code
      dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = 0)
    Output
      <SQL>
      SELECT
        `g`,
        MAX(CASE WHEN (`name` = 'x') THEN `value` WHEN NOT (`name` = 'x') THEN 0.0 END) AS `x`,
        MAX(CASE WHEN (`name` = 'y') THEN `value` WHEN NOT (`name` = 'y') THEN 0.0 END) AS `y`
      FROM `df`
      GROUP BY `g`

---

    Code
      dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = list(value = 0))
    Output
      <SQL>
      SELECT
        `g`,
        MAX(CASE WHEN (`name` = 'x') THEN `value` WHEN NOT (`name` = 'x') THEN 0.0 END) AS `x`,
        MAX(CASE WHEN (`name` = 'y') THEN `value` WHEN NOT (`name` = 'y') THEN 0.0 END) AS `y`
      FROM `df`
      GROUP BY `g`

# values_fill is checked

    Code
      dbplyr_pivot_wider_spec(lf, spec, values_fill = 1:2)
    Condition
      Error in `dbplyr_pivot_wider_spec()`:
      ! `values_fill` must be `NULL`, a scalar, or a named list, not an integer vector.

# cannot pivot lazy frames

    Code
      tidyr::pivot_wider(lazy_frame(name = "x", value = 1))
    Condition
      Error in `dbplyr_build_wider_spec()`:
      ! `dbplyr_build_wider_spec()` doesn't work with local lazy tibbles.
      i Use `memdb_frame()` together with `show_query()` to see the SQL code.

# `names_from` must be supplied if `name` isn't in `data` (#1240)

    Code
      (expect_error(tidyr::pivot_wider(df, values_from = val)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `dbplyr_build_wider_spec()`:
      ! Can't select columns that don't exist.
      x Column `name` doesn't exist.

# `values_from` must be supplied if `value` isn't in `data` (#1240)

    Code
      (expect_error(tidyr::pivot_wider(df, names_from = key)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `dbplyr_build_wider_spec()`:
      ! Can't select columns that don't exist.
      x Column `value` doesn't exist.

# `names_from` must identify at least 1 column (#1240)

    Code
      (expect_error(tidyr::pivot_wider(df, names_from = starts_with("foo"),
      values_from = val)))
    Output
      <error/rlang_error>
      Error in `dbplyr_build_wider_spec()`:
      ! `names_from` must select at least one column.

# `values_from` must identify at least 1 column (#1240)

    Code
      (expect_error(tidyr::pivot_wider(df, names_from = key, values_from = starts_with(
        "foo"))))
    Output
      <error/rlang_error>
      Error in `dbplyr_build_wider_spec()`:
      ! `values_from` must select at least one column.

# `id_expand` must be FALSE

    Code
      (expect_error(tidyr::pivot_wider(df, id_expand = TRUE)))
    Output
      <error/unsupported_arg_error>
      Error in `tidyr::pivot_wider()`:
      ! `id_expand = TRUE` isn't supported on database backends.
      i It must be FALSE instead.

