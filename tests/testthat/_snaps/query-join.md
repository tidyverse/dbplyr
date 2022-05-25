# print method doesn't change unexpectedly

    Code
      sql_build(left_join(lf1, lf2, by = "x") %>% left_join(lf3, by = c("x", "z")))
    Output
      <SQL JOINS>
      X:
        <IDENT> lf1
      Type: left
      By:
        x-x
      Y:
        <IDENT> lf2
      Type: left
      By:
        x-x
        z-z
      Y:
        <IDENT> lf3

---

    Code
      sql_build(right_join(lf1, lf2))
    Message
      Joining, by = "x"
    Output
      <SQL JOIN (RIGHT)>
      By:
        x-x
      X:
        <IDENT> lf1
      Y:
        <IDENT> lf2

# generated sql doesn't change unexpectedly

    Code
      inner_join(lf1, lf2)
    Message
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT `lf`.`x` AS `x`, `lf`.`y` AS `y`
      FROM `lf`
      INNER JOIN `lf2`
        ON (`lf`.`x` = `lf2`.`x` AND `lf`.`y` = `lf2`.`y`)

---

    Code
      left_join(lf1, lf2)
    Message
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT `lf`.`x` AS `x`, `lf`.`y` AS `y`
      FROM `lf`
      LEFT JOIN `lf2`
        ON (`lf`.`x` = `lf2`.`x` AND `lf`.`y` = `lf2`.`y`)

---

    Code
      right_join(lf1, lf2)
    Message
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT `lf2`.`x` AS `x`, `lf2`.`y` AS `y`
      FROM `lf`
      RIGHT JOIN `lf2`
        ON (`lf`.`x` = `lf2`.`x` AND `lf`.`y` = `lf2`.`y`)

---

    Code
      full_join(lf1, lf2)
    Message
      Joining, by = c("x", "y")
    Output
      <SQL>
      SELECT
        COALESCE(`lf`.`x`, `lf2`.`x`) AS `x`,
        COALESCE(`lf`.`y`, `lf2`.`y`) AS `y`
      FROM `lf`
      FULL JOIN `lf2`
        ON (`lf`.`x` = `lf2`.`x` AND `lf`.`y` = `lf2`.`y`)

# only disambiguates shared variables

    Code
      left_join(lf1, lf2)
    Message
      Joining, by = "x"
    Output
      <SQL>
      SELECT `lf`.`x` AS `x`, `y`, `z`
      FROM `lf`
      LEFT JOIN `lf2`
        ON (`lf`.`x` = `lf2`.`x`)

---

    Code
      left_join(lf1, lf2, by = c(y = "z"))
    Output
      <SQL>
      SELECT `lf`.`x` AS `x.x`, `y`, `lf2`.`x` AS `x.y`
      FROM `lf`
      LEFT JOIN `lf2`
        ON (`lf`.`y` = `lf2`.`z`)

# disambiguate variables that only differ in case

    Code
      left_join(lf1, lf2, by = "y")
    Output
      <SQL>
      SELECT `x`, `lf`.`y` AS `y`, `X`
      FROM `lf`
      LEFT JOIN `lf2`
        ON (`lf`.`y` = `lf2`.`y`)

# sql_on query doesn't change unexpectedly

    Code
      inner_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x.x`, `y`, `RHS`.`x` AS `x.y`, `z`
      FROM `lf` AS `LHS`
      INNER JOIN `lf2` AS `RHS`
        ON (LHS.y < RHS.z)

---

    Code
      left_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x.x`, `y`, `RHS`.`x` AS `x.y`, `z`
      FROM `lf` AS `LHS`
      LEFT JOIN `lf2` AS `RHS`
        ON (LHS.y < RHS.z)

---

    Code
      right_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x.x`, `y`, `RHS`.`x` AS `x.y`, `z`
      FROM `lf` AS `LHS`
      RIGHT JOIN `lf2` AS `RHS`
        ON (LHS.y < RHS.z)

---

    Code
      full_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT `LHS`.`x` AS `x.x`, `y`, `RHS`.`x` AS `x.y`, `z`
      FROM `lf` AS `LHS`
      FULL JOIN `lf2` AS `RHS`
        ON (LHS.y < RHS.z)

---

    Code
      semi_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT * FROM `lf` AS `LHS`
      WHERE EXISTS (
        SELECT 1 FROM `lf2` AS `RHS`
        WHERE (LHS.y < RHS.z)
      )

---

    Code
      anti_join(lf1, lf2, sql_on = "LHS.y < RHS.z")
    Output
      <SQL>
      SELECT * FROM `lf` AS `LHS`
      WHERE NOT EXISTS (
        SELECT 1 FROM `lf2` AS `RHS`
        WHERE (LHS.y < RHS.z)
      )

