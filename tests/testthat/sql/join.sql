$inner
<SQL>
SELECT `LHS`.`x` AS `x`, `LHS`.`y` AS `y`
FROM `df` AS `LHS`
INNER JOIN `df` AS `RHS`
ON (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)


$left
<SQL>
SELECT `LHS`.`x` AS `x`, `LHS`.`y` AS `y`
FROM `df` AS `LHS`
LEFT JOIN `df` AS `RHS`
ON (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)


$right
<SQL>
SELECT `RHS`.`x` AS `x`, `RHS`.`y` AS `y`
FROM `df` AS `LHS`
RIGHT JOIN `df` AS `RHS`
ON (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)


$full
<SQL>
SELECT COALESCE(`LHS`.`x`, `RHS`.`x`) AS `x`, COALESCE(`LHS`.`y`, `RHS`.`y`) AS `y`
FROM `df` AS `LHS`
FULL JOIN `df` AS `RHS`
ON (`LHS`.`x` = `RHS`.`x` AND `LHS`.`y` = `RHS`.`y`)


