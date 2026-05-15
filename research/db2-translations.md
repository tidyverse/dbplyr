# DB2 LUW - SQL translations for dbplyr

## Summary
Notes on DB2 LUW (Linux/Unix/Windows) SQL syntax needed for dbplyr translations.

## Type casts
DB2 supports standard `CAST(x AS type)`. Numeric types include `INTEGER`, `BIGINT`,
`DOUBLE`, `DECIMAL(p,s)`. String types include `VARCHAR(n)`, `CHAR(n)`, `CLOB`.

## String concatenation
DB2 supports the SQL-standard `||` operator and the `CONCAT()` function. Both
behave identically — `CONCAT(a, b)` is equivalent to `a || b`.

## Random numbers
`RAND()` returns a `DOUBLE` in the range `[0, 1)`. Equivalent to PostgreSQL's
`RANDOM()`.

## String search
- `LOCATE(search, source [, start])` — 1-indexed, returns 0 if not found.
- `INSTR(source, substring [, start [, occurrence]])` — synonym for
  `LOCATE_IN_STRING`.
- `POSITION(substring IN source)` — standard SQL syntax also supported.

## Regular expressions
DB2 11.1+ supports `REGEXP_REPLACE`, `REGEXP_LIKE`, `REGEXP_SUBSTR`,
`REGEXP_COUNT`, `REGEXP_INSTR`.
- `REGEXP_REPLACE(source, pattern, replacement [, start [, occurrence]])`. The
  default `occurrence = 0` replaces all matches.
- `REGEXP_LIKE(source, pattern)` — boolean predicate, can be used in `WHERE`.

## Date/time
- Current values: `CURRENT DATE`, `CURRENT TIMESTAMP` (no parens).
- Component extraction: `YEAR(x)`, `MONTH(x)`, `DAY(x)`, `HOUR(x)`, `MINUTE(x)`,
  `SECOND(x)`, `DAYOFWEEK(x)` (1=Sunday), `DAYOFYEAR(x)`, `WEEK(x)`,
  `WEEK_ISO(x)`, `QUARTER(x)`.
- Component extraction is also available via standard `EXTRACT(field FROM x)`.
- Date arithmetic uses labeled durations: `date + n DAYS`, `date + n MONTHS`,
  `date + n YEARS`. Note keywords are plural duration names.
- Difference in days: `DAYS(d2) - DAYS(d1)` where `DAYS()` converts a date to
  an integer day number.

## Aggregates
- `STDDEV_SAMP(x)` — sample standard deviation (matches R's `sd()`).
- `VAR_SAMP(x)` — sample variance.
- `CORRELATION(x, y)` (alias `CORR`).
- `COVARIANCE_SAMP(x, y)`.
- `LISTAGG(x, sep) WITHIN GROUP (ORDER BY ...)` — string aggregation
  (DB2 9.7+).

## Set operations
DB2 supports standard `UNION`, `INTERSECT`, `EXCEPT` (no need for Oracle-style
`MINUS`, though `EXCEPT` is preferred).

## Limit
DB2 uses `FETCH FIRST n ROWS ONLY` rather than `LIMIT`. Already implemented.

## Explain
`EXPLAIN PLAN FOR <sql>` writes to an EXPLAIN table; this is not directly
analogous to PostgreSQL `EXPLAIN`. Skipping for now.

## RUNSTATS
DB2 uses `RUNSTATS ON TABLE schema.table WITH DISTRIBUTION AND DETAILED INDEXES ALL`
rather than `ANALYZE`.

## Boolean
DB2 11.5+ supports a BOOLEAN type, but for safety we let `base_scalar` handle
TRUE/FALSE (rendered as `TRUE`/`FALSE`).

## Sources
- [DB2 CONCAT scalar function](https://www.ibm.com/docs/en/db2-for-zos/12.0.0?topic=functions-concat)
- [DB2 casting between data types](https://www.ibm.com/docs/en/db2-for-zos/12.0.0?topic=elements-casting-between-data-types)
- [DB2 REGEXP_REPLACE](https://www.ibm.com/docs/en/db2/11.5.x?topic=functions-regexp-replace)
- [DB2 REGEXP_LIKE](https://www.ibm.com/docs/en/db2/11.5.x?topic=functions-regexp-like)
- [DB2 LOCATE function](https://www.db2tutorial.com/db2-string-functions/db2-locate/)
- [DB2 INSTR function](https://www.ibm.com/docs/en/db2/11.1.0?topic=functions-instr)
- [DB2 RAND function](https://www.ibm.com/docs/en/db2/9.7?topic=functions-rand)
- [DB2 date functions](https://www.db2tutorial.com/db2-date-functions/)
- [DB2 LISTAGG function](https://www.db2tutorial.com/db2-aggregate-functions/db2-listagg/)
- [DB2 RUNSTATS](https://www.ibm.com/support/knowledgecenter/en/SSEPGG_11.1.0/com.ibm.db2.luw.admin.cmd.doc/doc/r0001980.html)
