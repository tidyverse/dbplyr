<SQL>
SELECT "x", "x" + 1.0 AS "y"
FROM "dbplyr_001"

<PLAN>
Seq Scan on dbplyr_001  (cost=0.00..1.04 rows=3 width=36)
