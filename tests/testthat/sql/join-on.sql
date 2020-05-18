$inner
<SQL>
SELECT `LHS`.`x` AS `x.x`, `LHS`.`y` AS `y`, `RHS`.`x` AS `x.y`, `RHS`.`z` AS `z`
FROM `df` AS `LHS`
INNER JOIN `df` AS `RHS`
ON (LHS.y < RHS.z)


$left
<SQL>
SELECT `LHS`.`x` AS `x.x`, `LHS`.`y` AS `y`, `RHS`.`x` AS `x.y`, `RHS`.`z` AS `z`
FROM `df` AS `LHS`
LEFT JOIN `df` AS `RHS`
ON (LHS.y < RHS.z)


$right
<SQL>
SELECT `LHS`.`x` AS `x.x`, `LHS`.`y` AS `y`, `RHS`.`x` AS `x.y`, `RHS`.`z` AS `z`
FROM `df` AS `LHS`
RIGHT JOIN `df` AS `RHS`
ON (LHS.y < RHS.z)


$full
<SQL>
SELECT `LHS`.`x` AS `x.x`, `LHS`.`y` AS `y`, `RHS`.`x` AS `x.y`, `RHS`.`z` AS `z`
FROM `df` AS `LHS`
FULL JOIN `df` AS `RHS`
ON (LHS.y < RHS.z)


$semi
<SQL>
SELECT * FROM `df` AS `LHS`
WHERE EXISTS (
  SELECT 1 FROM `df` AS `RHS`
  WHERE (LHS.y < RHS.z)
)

$anti
<SQL>
SELECT * FROM `df` AS `LHS`
WHERE NOT EXISTS (
  SELECT 1 FROM `df` AS `RHS`
  WHERE (LHS.y < RHS.z)
)

