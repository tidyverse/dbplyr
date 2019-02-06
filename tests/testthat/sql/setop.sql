$union
<SQL>
(SELECT *
FROM `df`)
UNION
(SELECT *
FROM `df`)

$setdiff
<SQL>
(SELECT *
FROM `df`)
EXCEPT
(SELECT *
FROM `df`)

$intersect
<SQL>
(SELECT *
FROM `df`)
INTERSECT
(SELECT *
FROM `df`)

