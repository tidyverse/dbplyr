# can pivot all cols to wide

    Code
      lazy_frame(key = c("x", "y", "z"), val = 1:3) %>% dbplyr_pivot_wider_spec(spec)
    Output
      <SQL>
      SELECT MAX(CASE WHEN (`key` = 'x') THEN (`val`) END) AS `x`, MAX(CASE WHEN (`key` = 'y') THEN (`val`) END) AS `y`, MAX(CASE WHEN (`key` = 'z') THEN (`val`) END) AS `z`
      FROM `df`

# implicit missings turn into explicit missings

    Code
      lazy_frame(a = 1:2, key = c("x", "y"), val = 1:2) %>% dbplyr_pivot_wider_spec(
        spec)
    Output
      <SQL>
      SELECT `a`, MAX(CASE WHEN (`key` = 'x') THEN (`val`) END) AS `x`, MAX(CASE WHEN (`key` = 'y') THEN (`val`) END) AS `y`
      FROM `df`
      GROUP BY `a`

# values_fn can be a single function

    Code
      dbplyr_pivot_wider_spec(df, spec1, values_fn = sum)
    Output
      <SQL>
      SELECT `a`, SUM(CASE WHEN (`key` = 'x') THEN (`val`) END) AS `x`
      FROM `df`
      GROUP BY `a`

# values_fn cannot be NULL

    Code
      expect_error(dbplyr_pivot_wider_spec(df, spec1, values_fn = NULL))

# can fill in missing cells

    Code
      dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = 0)
    Output
      <SQL>
      SELECT `g`, `name`, `value`, MAX(CASE WHEN (`key` = 'x') THEN (`val`) WHEN NOT(`key` = 'x') THEN (0.0) END) AS `x`, MAX(CASE WHEN (`key` = 'y') THEN (`val`) WHEN NOT(`key` = 'y') THEN (0.0) END) AS `y`
      FROM `df`
      GROUP BY `g`, `name`, `value`

---

    Code
      dbplyr_pivot_wider_spec(df_lazy, spec, values_fill = list(value = 0))
    Output
      <SQL>
      SELECT `g`, `name`, `value`, MAX(CASE WHEN (`key` = 'x') THEN (`val`) END) AS `x`, MAX(CASE WHEN (`key` = 'y') THEN (`val`) END) AS `y`
      FROM `df`
      GROUP BY `g`, `name`, `value`

