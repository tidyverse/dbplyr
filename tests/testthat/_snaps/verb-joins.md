# complete join pipeline works with SQLite and table alias

    Code
      remote_query(out)
    Output
      <SQL> SELECT `df1`.`x` AS `x`, `y`
      FROM `dbplyr_068` AS `df1`
      LEFT JOIN `dbplyr_069` AS `df2`
      ON (`df1`.`x` = `df2`.`x`)
      

# join check `lhs_as` and `rhs_as`

    Code
      left_join(x, x, by = "x", lhs_as = NULL)
    Error <vctrs_error_scalar_type>
      `lhs_as` must be a vector, not NULL.

---

    Code
      left_join(x, x, by = "x", rhs_as = c("A", "B"))
    Error <vctrs_error_assert_size>
      `rhs_as` must have size 1, not size 2.

# can optionally match NA values

    Code
      left_join(lf, lf, by = "x", na_matches = "na")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x`
      FROM `df` AS `LHS`
      LEFT JOIN `df` AS `RHS`
      ON (CASE WHEN (`LHS`.`x` = `RHS`.`x`) OR (`LHS`.`x` IS NULL AND `RHS`.`x` IS NULL) THEN 0 ELSE 1 = 0)
      

