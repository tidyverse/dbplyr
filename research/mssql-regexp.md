# SQL Server 2025 - Regular Expression Functions

## Summary

SQL Server 2025 introduces native regex functions: REGEXP_LIKE, REGEXP_REPLACE, REGEXP_SUBSTR, and REGEXP_COUNT. These enable dbplyr translations for stringr functions like `str_detect()`, `str_replace()`, `str_extract()`, and `str_count()`.

## Compatibility

- Requires SQL Server 2025 (17.x) or later
- REGEXP_LIKE requires compatibility level 170 or above
- Other functions available at all compatibility levels
- Uses RE2 regex syntax (not PCRE)

## Functions

### REGEXP_LIKE

```sql
REGEXP_LIKE(string_expression, pattern_expression [, flags])
```

- Returns: `boolean` (true/false)
- Flags: `i` (case-insensitive), `c` (case-sensitive, default), `m` (multiline), `s` (dotall)
- NULL handling: Standard SQL NULL propagation

### REGEXP_REPLACE

```sql
REGEXP_REPLACE(string_expression, pattern_expression [, replacement [, start [, occurrence [, flags]]]])
```

- Returns: Modified string (or original if no match)
- Default replacement: `''` (empty string)
- Default start: `1` (1-indexed)
- Default occurrence: `0` (replace all); use `1` for first only
- Flags: Same as REGEXP_LIKE
- Backreferences: `\1` through `\9` for captured groups; `&` for full match

### REGEXP_SUBSTR

```sql
REGEXP_SUBSTR(string_expression, pattern_expression [, start [, occurrence [, flags [, group]]]])
```

- Returns: Matched substring (or NULL if no match)
- Default start: `1` (1-indexed)
- Default occurrence: `1` (first match)
- Default group: `0` (entire match); use `1-9` for capture groups
- Flags: Same as REGEXP_LIKE

### REGEXP_COUNT

```sql
REGEXP_COUNT(string_expression, pattern_expression [, start [, flags]])
```

- Returns: `int` (count of matches, 0 if none)
- Default start: `1` (1-indexed)
- Flags: Same as REGEXP_LIKE

## Limitations

- Pattern max length: 8,000 bytes
- String max length: 2 MB for LOB types (varchar(max), nvarchar(max))
- Contradictory flags: Last flag wins (e.g., `ic` → case-sensitive)

## Sources

- [REGEXP_LIKE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/functions/regexp-like-transact-sql?view=sql-server-ver17)
- [REGEXP_REPLACE (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/functions/regexp-replace-transact-sql?view=sql-server-ver17)
- [REGEXP_SUBSTR (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/functions/regexp-substr-transact-sql?view=sql-server-ver17)
- [REGEXP_COUNT (Transact-SQL)](https://learn.microsoft.com/en-us/sql/t-sql/functions/regexp-count-transact-sql?view=sql-server-ver17)
- [GA Announcement: Regex Support in SQL Server 2025 & Azure SQL](https://techcommunity.microsoft.com/blog/azuresqlblog/general-availability-announcement-regex-support-in-sql-server-2025--azure-sql/4470684)
