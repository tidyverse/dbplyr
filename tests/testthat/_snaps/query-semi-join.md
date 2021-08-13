# print method doesn't change unexpectedly

    Code
      sql_build(semi_join(lf1, lf2))
    Message <message>
      Joining, by = "x"
    Output
      <SQL SEMI JOIN>
      By:
        x-x
      X:
        <IDENT> df
      Y:
        <IDENT> df

# generated sql doesn't change unexpectedly

    Code
      semi_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT * FROM `df` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM `df` AS `RHS`
        WHERE (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)
      )

---

    Code
      anti_join(lf, lf)
    Message <message>
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT * FROM `df` AS `LHS`
      WHERE NOT EXISTS (
        SELECT 1 FROM `df` AS `RHS`
        WHERE (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)
      )

# CTEs work with join

    Code
      semi_join(lf1, lf2, by = "x") %>% remote_query(cte = TRUE)
    Output
      <SQL> SELECT * FROM `df1` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM `df2` AS `RHS`
        WHERE (`LHS`.`x` = `RHS`.`x`)
      )

---

    Code
      semi_join(lf1_f, lf2_f, by = "x") %>% remote_query(cte = TRUE)
    Output
      <SQL> WITH `q01` AS (
      SELECT *
      FROM `df1`
      WHERE (`x` = 1.0)
      ),
      `q02` AS (
      SELECT *
      FROM `df2`
      WHERE (`x` = 1.0)
      )
      SELECT * FROM `q01` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM `q02` AS `RHS`
        WHERE (`LHS`.`x` = `RHS`.`x`)
      )

---

    Code
      semi_join(lf1_f, lf1_f, by = "x") %>% remote_query(cte = TRUE)
    Output
      <SQL> WITH `q01` AS (
      SELECT *
      FROM `df1`
      WHERE (`x` = 1.0)
      )
      SELECT * FROM `q01` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM `q01` AS `RHS`
        WHERE (`LHS`.`x` = `RHS`.`x`)
      )

