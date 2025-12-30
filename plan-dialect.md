# SQL Dialect System

## Status: Core Implementation Complete

The `sql_dialect()` system has been implemented. All backends have been migrated to use dialect objects, and the old `supports_window_clause()` and `db_supports_table_alias_with_as()` generics have been removed.

## Completed Work

### 1. Core Infrastructure (R/dialect.R)

- Created `sql_dialect()` generic with `sql_dialect.DBIConnection` fallback for backward compatibility
- Created `new_sql_dialect()` constructor with:
  - `dialect` - string name
  - `quote_identifier` - function for identifier quoting
  - `supports_window_clause` - boolean (default: FALSE)
  - `supports_table_alias_with_as` - boolean (default: TRUE)
- Added helper functions:
  - `sql_has_window_clause(con)` - safely extracts the field from dialect or returns FALSE for plain connections

  - `sql_has_table_alias_with_as(con)` - safely extracts the field from dialect or returns TRUE for TestConnection, FALSE for plain connections

### 2. Iterated Dispatch Pattern

Added iterated dispatch to the following generics (pattern: call `sql_dialect(con)`, then dispatch on result):

- `sql_translation()`
- `sql_expr_matches()`
- `sql_query_select()`
- `sql_query_explain()`
- `sql_query_wrap()`
- `sql_query_set_op()`
- `sql_query_multi_join()`
- `sql_query_append()`
- `sql_query_delete()`
- `sql_values_subquery()`
- `sql_table_analyze()`
- `sql_escape_ident()`
- `sql_escape_logical()`
- `sql_escape_date()`
- `sql_escape_datetime()`
- `sql_escape_raw()`
- `table_path_components()`

### 3. Backend Migration

All backends now define `dialect_*()` functions and `sql_dialect.*` methods:

| Backend | Dialect Function | Quote Style | Window Clause | Table Alias AS |
|---------|------------------|-------------|---------------|----------------|
| PostgreSQL | `dialect_postgres()` | `"x"` | TRUE | TRUE (default) |
| MySQL | `dialect_mysql()` | `` `x` `` | TRUE | FALSE |
| MariaDB | `dialect_mariadb()` | `` `x` `` | TRUE | TRUE (default) |
| SQLite | `dialect_sqlite()` | `` `x` `` | TRUE | TRUE (default) |
| SQL Server | `dialect_mssql()` | `[x]` | FALSE (default) | TRUE (default) |
| Oracle | `dialect_oracle()` | `"x"` | FALSE (default) | FALSE |
| Redshift | `dialect_redshift()` | `"x"` | FALSE (default) | TRUE (default) |
| Snowflake | `dialect_snowflake()` | `"x"` | TRUE | TRUE (default) |
| Hive | `dialect_hive()` | `"x"` | TRUE | TRUE (default) |
| Impala | `dialect_impala()` | `"x"` | TRUE | TRUE (default) |
| Spark SQL | `dialect_spark_sql()` | `"x"` | TRUE | TRUE (default) |
| Teradata | `dialect_teradata()` | `"x"` | TRUE | TRUE (default) |
| SAP HANA | `dialect_hana()` | `"x"` | FALSE (default) | TRUE (default) |
| MS Access | `dialect_access()` | `[x]` | TRUE | TRUE (default) |

### 4. Test Migration

All tests now use `dialect_*()` functions instead of `simulate_*()`:
- Created `dialect_ansi()` for generic ANSI SQL testing (replaces `simulate_dbi()`)
- Created `dialect_odbc()` for ODBC testing
- Updated all test files via sed: `simulate_*()` → `dialect_*()`

### 5. Additional sql_dialect Methods

Added `*.sql_dialect` methods for generics used by tests:
- `db_sql_render.sql_dialect`
- `sql_join_suffix.sql_dialect`
- `sql_table_index.sql_dialect`
- `sql_query_rows.sql_dialect`
- `db_table_temporary.sql_dialect`
- `dbplyr_fill0.sql_dialect`
- `sql_query_semi_join.sql_dialect`

MSSQL-specific methods for dialect dispatch:
- `db_sql_render.sql_dialect_mssql` (handles BIT to BOOLEAN conversion in WHERE clauses)
- `db_table_temporary.sql_dialect_mssql` (handles temporary table prefix)

### 6. Removed Code

- Deleted `supports_window_clause()` generic and all methods
- Deleted `db_supports_table_alias_with_as()` generic and all methods
- Deleted `sql_escape_ident.TestConnection` (now uses dialect dispatch)
- Removed duplicate method aliases (e.g., `sql_translation.PostgreSQL <- sql_translation.PqConnection`)

### 7. Special Cases

- **MSSQL**: `dialect_mssql(version)` accepts a version parameter, stores it in `dialect$version`
- **Access/MSSQL**: Custom `table_path_components.sql_dialect_*` methods handle asymmetric `[x]` bracket quoting
- **TestConnection**: `sql_has_table_alias_with_as()` returns TRUE for TestConnection to preserve test behavior

## Remaining Work

### Not Yet Done

1. **Deprecate `simulate_*()` functions** - Currently still work via `simulate_dbi()` creating TestConnection objects. Could deprecate in favor of `dialect_*()` functions.

2. **Documentation updates**:
   - Update `vignette("new-backend")` to show dialect approach
   - Add `dialect_*()` functions to `_pkgdown.yml` reference index

3. **ADBC support** - The infrastructure is in place but ADBC backends have not been migrated yet.

## Original Design Goals

### Goal

Implement a `sql_dialect()` system to reduce method duplication across database backends by:
1. ✅ Creating a dialect object that acts as an (optional) intermediary between the database connection and SQL translation.
2. ✅ Allowing multiple connection classes to share a single dialect.
3. ✅ Maintaining backward compatibility for user-facing APIs

### Problems Solved

- ✅ PostgreSQL `PqConnection` and `PostgreSQL` classes now share `dialect_postgres()`
- ✅ MySQL `MySQLConnection` and `MariaDBConnection` now share `dialect_mysql()`/`dialect_mariadb()`
- ✅ Eliminated ~60 alias definitions across backends
- ✅ Infrastructure ready for ODBC/ADBC to dispatch to appropriate dialect based on connection metadata
