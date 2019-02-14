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


