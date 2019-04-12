<SQL>
SELECT "x", "x" + 1.0 AS "y"
FROM "dbplyr_002"

<PLAN>
Seq Scan on dbplyr_002  (cost=0.00..1.04 rows=3 width=36)
