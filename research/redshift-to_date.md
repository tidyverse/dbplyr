# Redshift - TO_DATE

## Summary
Converts a string to DATE type using a format string for dbplyr's date_build translation.

## Syntax
```sql
TO_DATE(string, format)
TO_DATE(string, format, is_strict)
```

## Parameters
- `string`: Character string to convert
- `format`: String literal defining date format (e.g., 'YYYY-MM-DD')
- `is_strict` (optional): Boolean for error handling (default FALSE allows overflow)

## Key behaviors
- Returns DATE type
- Does not support format string with Q (Quarter number)
- With `is_strict=FALSE` (default), accepts overflow values (e.g., '2001-06-31' becomes '2001-07-01')
- With `is_strict=TRUE`, raises error for out-of-range dates

## Examples
```sql
-- Basic conversion
to_date('02 Oct 2001', 'DD Mon YYYY')  -- Returns 2001-10-02

-- Using YYYY-MM-DD format
to_date('2001-10-02', 'YYYY-MM-DD')

-- String concatenation with CAST and || operator
TO_DATE(CAST(2020 AS TEXT) || '-' || CAST(1 AS TEXT) || '-' || CAST(15 AS TEXT), 'YYYY-MM-DD')
```

## Sources
- [TO_DATE function - Amazon Redshift](https://docs.aws.amazon.com/redshift/latest/dg/r_TO_DATE_function.html)
- [+ (Concatenation) operator - Amazon Redshift](https://docs.aws.amazon.com/redshift/latest/dg/r_DATE-CONCATENATE_function.html)
