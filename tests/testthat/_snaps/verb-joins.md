# complete join pipeline works with SQLite and table alias

    Code
      left_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2")
    Output
      <SQL>
      SELECT `df1`.`x` AS `x`, `y`
      FROM `lf1` AS `df1`
      LEFT JOIN `lf2` AS `df2`
        ON (`df1`.`x` = `df2`.`x`)

# complete semi join works with SQLite and table alias

    Code
      inner_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2")
    Output
      <SQL>
      SELECT `df1`.*
      FROM `df` AS `df1`
      INNER JOIN `df` AS `df2`
        ON (`df1`.`x` = `df2`.`x`)

# join check `x_as` and `y_as`

    Code
      left_join(x, x, by = "x", y_as = c("A", "B"))
    Condition
      Error in `left_join()`:
      ! `y_as` must have size 1, not size 2.

---

    Code
      left_join(x, x, by = "x", x_as = "LHS", y_as = "LHS")
    Condition
      Error in `left_join()`:
      ! `y_as` must be different from `x_as`.

---

    Code
      left_join(x, y, by = "a", x_as = "y")
    Condition
      Error in `left_join()`:
      ! `y_as` must be different from `x_as`.

---

    Code
      left_join(x %>% filter(x == 1), x, by = "x", y_as = "LHS")
    Condition
      Error in `left_join()`:
      ! `y_as` must be different from `x_as`.

# select() before join is inlined

    Code
      out_left
    Output
      <SQL>
      SELECT `a` AS `a2`, `x1` AS `x`, `b`
      FROM `lf1`
      LEFT JOIN `lf2`
        ON (`lf1`.`x1` = `lf2`.`x2`)

# select() before semi_join is inlined

    Code
      out_semi
    Output
      <SQL>
      SELECT `a` AS `a2`, `x1` AS `x`
      FROM `lf1`
      WHERE EXISTS (
        SELECT 1 FROM (
        SELECT `x2` AS `x`, `b`
        FROM `lf2`
      ) `RHS`
        WHERE (`lf1`.`x1` = `RHS`.`x`)
      )

# can optionally match NA values

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `df_LHS`.`x` AS `x`
      FROM `df` AS `df_LHS`
      LEFT JOIN `df` AS `df_RHS`
        ON (CASE WHEN (`df_LHS`.`x` = `df_RHS`.`x`) OR (`df_LHS`.`x` IS NULL AND `df_RHS`.`x` IS NULL) THEN 0 ELSE 1 END = 0)

# suffix arg is checked

    Code
      inner_join(lf1, lf2, by = "x", suffix = "a")
    Condition
      Error in `inner_join()`:
      ! `suffix` must have size 2, not size 1.

# joins reuse queries in cte mode

    Code
      left_join(lf, lf) %>% remote_query(cte = TRUE)
    Message
      Joining, by = "x"
    Output
      <SQL> WITH `q01` AS (
        SELECT `lf1_LHS`.`x` AS `x`
        FROM `lf1` AS `lf1_LHS`
        INNER JOIN `lf1` AS `lf1_RHS`
          ON (`lf1_LHS`.`x` = `lf1_RHS`.`x`)
      )
      SELECT `LHS`.`x` AS `x`
      FROM `q01` AS `LHS`
      LEFT JOIN `q01` AS `RHS`
        ON (`LHS`.`x` = `RHS`.`x`)

