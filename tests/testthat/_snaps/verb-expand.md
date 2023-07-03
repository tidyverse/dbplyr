# expand completes all values

    Code
      lazy_frame(x = 1, y = 1) %>% tidyr::expand(x, y)
    Output
      <SQL>
      SELECT `x`, `y`
      FROM (
        SELECT DISTINCT `x`
        FROM `df`
      ) AS `LHS`
      CROSS JOIN (
        SELECT DISTINCT `y`
        FROM `df`
      ) AS `RHS`

# nesting doesn't expand values

    Code
      df_lazy %>% tidyr::expand(nesting(x, y))
    Output
      <SQL>
      SELECT DISTINCT `df`.*
      FROM `df`

# expand accepts expressions

    Code
      tidyr::expand(df, round(x / 2))
    Output
      <SQL>
      SELECT DISTINCT ROUND(`x` / 2.0, 0) AS `round(x/2)`
      FROM `df`

---

    Code
      tidyr::expand(df, nesting(x_half = round(x / 2), x1 = x + 1))
    Output
      <SQL>
      SELECT DISTINCT ROUND(`x` / 2.0, 0) AS `x_half`, `x` + 1.0 AS `x1`
      FROM `df`

# works with tidyr::nesting

    Code
      df_lazy %>% tidyr::expand(tidyr::nesting(x, y))
    Output
      <SQL>
      SELECT DISTINCT `df`.*
      FROM `df`

# expand respects groups

    Code
      df_lazy %>% group_by(a) %>% tidyr::expand(b, c)
    Output
      <SQL>
      SELECT `LHS`.*, `c`
      FROM (
        SELECT DISTINCT `a`, `b`
        FROM `df`
      ) AS `LHS`
      LEFT JOIN (
        SELECT DISTINCT `a`, `c`
        FROM `df`
      ) AS `RHS`
        ON (`LHS`.`a` = `RHS`.`a`)

# NULL inputs

    Code
      tidyr::expand(lazy_frame(x = 1), x, y = NULL)
    Output
      <SQL>
      SELECT DISTINCT `df`.*
      FROM `df`

# expand() errors when expected

    Code
      tidyr::expand(memdb_frame(x = 1))
    Condition
      Error in `tidyr::expand()`:
      ! Must supply variables in `...`

---

    Code
      tidyr::expand(memdb_frame(x = 1), x = NULL)
    Condition
      Error in `tidyr::expand()`:
      ! Must supply variables in `...`

# nesting() respects .name_repair

    Code
      tidyr::expand(memdb_frame(x = 1, y = 1), nesting(x, x = x + 1))
    Condition
      Error in `tidyr::expand()`:
      ! In expression `nesting(x, x = x + 1)`:
      Caused by error:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.

# replace_na replaces missing values

    Code
      lazy_frame(x = 1, y = "a") %>% tidyr::replace_na(list(x = 0, y = "unknown"))
    Output
      <SQL>
      SELECT COALESCE(`x`, 0.0) AS `x`, COALESCE(`y`, 'unknown') AS `y`
      FROM `df`

# replace_na ignores missing columns

    Code
      lazy_frame(x = 1) %>% tidyr::replace_na(list(not_there = 0))
    Output
      <SQL>
      SELECT *
      FROM `df`

# complete completes missing combinations

    Code
      df_lazy %>% tidyr::complete(x, y, fill = list(z = "c"))
    Output
      <SQL>
      SELECT `x`, `y`, COALESCE(`z`, 'c') AS `z`
      FROM (
        SELECT
          COALESCE(`LHS`.`x`, `df`.`x`) AS `x`,
          COALESCE(`LHS`.`y`, `df`.`y`) AS `y`,
          `z`
        FROM (
          SELECT `x`, `y`
          FROM (
            SELECT DISTINCT `x`
            FROM `df`
          ) AS `LHS`
          CROSS JOIN (
            SELECT DISTINCT `y`
            FROM `df`
          ) AS `RHS`
        ) AS `LHS`
        FULL JOIN `df`
          ON (`LHS`.`x` = `df`.`x` AND `LHS`.`y` = `df`.`y`)
      ) AS `q01`

