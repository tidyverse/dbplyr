# Redshift - IS NOT DISTINCT FROM in ON clause

## Summary

Redshift does NOT support the `IS NOT DISTINCT FROM` operator in JOIN ON clauses (or anywhere else). This PostgreSQL operator is not available in Redshift, requiring workarounds for null-safe comparisons in joins.

## Syntax

NOT SUPPORTED in Redshift:
```sql
-- This does NOT work in Redshift
SELECT *
FROM a
JOIN b ON a.col IS NOT DISTINCT FROM b.col
```

WORKAROUND - Explicit NULL check:
```sql
SELECT *
FROM a
JOIN b ON (a.col = b.col) OR (a.col IS NULL AND b.col IS NULL)
```

WORKAROUND - Using COALESCE:
```sql
SELECT *
FROM a
JOIN b ON COALESCE(a.col, 'sentinel_value') = COALESCE(b.col, 'sentinel_value')
```

## Key behaviors

- Standard equality (`=`) in Redshift returns NULL when either operand is NULL
- NULL does not equal NULL in standard SQL comparisons
- Redshift does not match rows in joins when join columns contain NULL, even if both sides are NULL
- For multi-column joins, each column needs its own NULL handling

## Limitations

- No native null-safe comparison operator
- Must use verbose workarounds that increase query complexity
- COALESCE approach requires choosing a sentinel value that won't occur naturally in the data
- Explicit NULL check pattern becomes unwieldy with multiple join columns

## Sources

- [Amazon Redshift: Comparison condition](https://docs.aws.amazon.com/redshift/latest/dg/r_comparison_condition.html)
- [Amazon Redshift: Nulls](https://docs.aws.amazon.com/redshift/latest/dg/r_Nulls.html)
- [PopSQL: How to Compare Two Values When One is Null in Redshift](https://popsql.com/learn-sql/redshift/how-to-compare-two-values-when-one-is-null-in-redshift)
- [Stack Overflow: Redshift left outer join is leaving out nulls](https://stackoverflow.com/questions/28080883/redshift-left-outer-join-is-leaving-out-nulls)
