$inplace
<SQL>
SELECT `x` + 1.0 AS `x`
FROM (SELECT `x` + 1.0 AS `x`
FROM `df`)

$increment
<SQL>
SELECT `x`, `x1`, `x1` + 1.0 AS `x2`
FROM (SELECT `x`, `x` + 1.0 AS `x1`
FROM `df`)

