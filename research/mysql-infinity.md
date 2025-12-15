# MySQL - Infinity

## Summary

MySQL does **not support** IEEE 754 infinity values (positive or negative infinity) for FLOAT or DOUBLE data types. The SQL standard defines infinity and NaN as invalid values for these types, and MySQL adheres to this standard.

## Behavior

### Arithmetic operations

Division operations that would produce infinity in IEEE 754 return NULL instead:

```sql
SELECT 1/0, 0/0;
-- Result: NULL, NULL (not Inf, NaN)
```

### INSERT operations

Attempting to insert infinity values results in conversion to 0.0 or errors:

- Inserting string literals 'Inf', '+Inf', '-Inf', or 'Infinity' typically results in 0.0 being stored
- Depending on SQL mode, may generate Error Code 1265: "Data truncated for column" warnings
- In strict SQL modes, the INSERT may fail entirely

### SELECT operations (platform-dependent)

When very large values are stored (e.g., `1e+52`), behavior is platform-dependent:

- Some platforms: SELECT returns `inf` and `-inf`
- Other platforms: SELECT returns `0` and `-0`

This inconsistency is documented in the MySQL reference manual as platform or implementation dependent behavior.

## Workarounds

Since MySQL lacks native infinity support, common workarounds include:

1. **Large numbers**: Use arbitrarily large values (e.g., `1e308` for positive, `-1e308` for negative)
2. **Language constants**: Use `Double.MAX_VALUE` / `Double.MIN_VALUE` from application code
3. **NULL values**: Represent infinity as NULL with additional flag columns
4. **Separate column**: Add a VARCHAR column to store text representations ('Infinity', '-Infinity') while the numeric column stores NULL
5. **Application layer**: Convert infinity values to/from alternative representations in application code before storage and after retrieval

## Recommendations

For applications requiring exact numeric values or special value handling:

- Use DECIMAL type for exact values (no special values, but no approximation either)
- Handle infinity cases at the application layer before inserting into MySQL
- Document clearly how infinity is represented in your schema

## Contrast with other databases

Some database systems do support infinity:

- **PostgreSQL**: Supports 'Infinity', '-Infinity', and 'NaN' for DOUBLE PRECISION and REAL types
- **Apache Doris**: Supports Inf, -Inf, and NaN conforming to IEEE 754

## Sources

- [MySQL 8.4 Reference Manual: Floating-Point Types](https://dev.mysql.com/doc/refman/8.4/en/floating-point-types.html)
- [MySQL 8.4 Reference Manual: Problems with Floating-Point Values](https://dev.mysql.com/doc/refman/8.4/en/problems-with-float.html)
- [MySQL Bug #80953: Documentation over Infinity vs NaN](https://bugs.mysql.com/bug.php?id=80953)
- [Stack Overflow: MySQL IEEE floating point NaN, PositiveInfinity, NegativeInfinity](https://stackoverflow.com/questions/41936403/mysql-ieee-floating-point-nan-positiveinfinity-negativeinfinity)
- [Database Administrators Stack Exchange: Does MySQL support '+/- infinity' numeric value?](https://dba.stackexchange.com/questions/338224/does-mysql-support-infinity-numeric-value)
- [Stack Overflow: Storing Double.POSITIVE_INFINITY in MySQL](https://stackoverflow.com/questions/5807749/storing-double-positive-infinity-in-mysql-ejb-entity-jboss)
