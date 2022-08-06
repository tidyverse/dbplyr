# complete join pipeline works with SQLite and table alias

    Code
      left_join(lf1, lf2, by = "x", x_as = "df1", y_as = "df2")
    Output
      <SQL>
      SELECT `df1`.`x` AS `x`, `y`
      FROM `df` AS `df1`
      LEFT JOIN `df` AS `df2`
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
      left_join(x, x, by = "x", x_as = NULL)
    Condition
      Error in `left_join()`:
      ! `x_as` must be a vector, not NULL.

---

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

# select() before join is inlined

    Code
      out_left
    Output
      <SQL>
      SELECT `a` AS `a2`, `x1` AS `x`, `b`
      FROM `lf1` AS `LHS`
      LEFT JOIN `lf2` AS `RHS`
        ON (`LHS`.`x1` = `RHS`.`x2`)

# select() before semi_join is inlined

    Code
      out_semi
    Output
      <SQL>
      SELECT `a` AS `a2`, `x1` AS `x`
      FROM `lf1` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM (
        SELECT `x2` AS `x`, `b`
        FROM `lf2`
      ) `RHS`
        WHERE (`LHS`.`x1` = `RHS`.`x`)
      )

# can optionally match NA values

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
        ON (CASE WHEN (`LHS`.`x` = `RHS`.`x`) OR (`LHS`.`x` IS NULL AND `RHS`.`x` IS NULL) THEN 0 ELSE 1 END = 0)

# suffix arg is checked

    Code
      inner_join(lf1, lf2, by = "x", suffix = "a")
    Condition
      Error in `inner_join()`:
      ! `suffix` must have size 2, not size 1.

