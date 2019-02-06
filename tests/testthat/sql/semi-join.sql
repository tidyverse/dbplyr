$semi
<SQL>
SELECT * FROM `df` AS `LHS`
WHERE EXISTS (
  SELECT 1 FROM `df` AS `RHS`
  WHERE (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)
)

$anti
<SQL>
SELECT * FROM `df` AS `LHS`
WHERE NOT EXISTS (
  SELECT 1 FROM `df` AS `RHS`
  WHERE (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)
)

