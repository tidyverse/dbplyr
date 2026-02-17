# Revdep analysis

## bigrquery (1.6.1)

3 test failures. Tests inspect `sql$select[[n]]` and expect bare expressions like `"SAFE_CAST(\`x\` AS FLOAT64)"` but now get aliased expressions like `"SAFE_CAST(\`x\` AS FLOAT64) AS \`z\`"` with class `c("sql", "character")` instead of plain character. The tests are too tightly coupled to dbplyr's internal select clause representation. bigrquery needs to update its test expectations.

## CohortCharacteristics (1.1.0)

1 test failure. `summariseCohortOverlap()` produces SQL with an ambiguous column reference: `cohort_definition_id` exists in both `pp_test_table` and `pp_test_table_set` after a join but is referenced without a table qualifier. DuckDB's binder rejects this. Likely caused by a change in how dbplyr qualifies column references in joins.

## dbi.table (1.0.6)

1 test failure. Test expects `dbplyr::translate_sql_()` to throw an error, but the backward-compat branch restored this function. This is a **false positive** -- the test was defensively checking that `translate_sql_()` was broken, and restoring it causes the assumption to fail. Fix belongs in dbi.table.

## diseasystore (0.3.2)

22 test failures + vignette error via SCDB dependency. **Fixed** by restoring `DBI::Id` handling in `as.sql()`.

## dm (1.0.12)

2 errors (examples + tests):

1. **Examples**: `DBI::dbUnquoteIdentifier()` fails on table names -- the format of `remote_name` changed so it can no longer be unquoted.
2. **Tests**: Setup fails because `tbl_sum.tbl_sql` method no longer exists, breaking `testthat::local_mocked_bindings()`.

`dbplyr_connection_describe()` restored, which may fix the `tbl_sum` issue if dm was using it indirectly.

## etl (0.4.2)

1 test failure. `print(cars)` output no longer contains `"sqlite"` (lowercase). The print representation of database-backed objects changed. The test has a fragile regex assertion on print output. Fix belongs in etl.

## healthdb (0.5.0)

13 test failures. All fail with SQLite error `"row value misused"`. The `identify_rows()` and `define_case()` functions use `if_any()`/`if_all()` with `%in%`, and dbplyr now generates SQL with row value expressions that SQLite doesn't support. Previously this was likely expanded into individual `OR`-ed conditions.

## lazysf (0.2.0)

1 example error. `SFSQLConnection` uses dbplyr's 1st edition interface, which is no longer supported. lazysf maintainer needs to update to 2nd edition.

## mlr3db (0.7.0)

3 test failures. All fail with `"bad_weak_ptr"` from RSQLite -- the C++ pointer inside the connection is invalid (stale/serialized connection). Tests involve reconnection and parallel execution scenarios. Likely an mlr3db issue with connection management, exposed by changes in dbplyr's error handling.

## osdc (0.9.19)

1 test failure. DuckDB temporary table `as_tbl_duckplyr_XMUENgxduz` does not exist when the final query is collected. A different table name exists (`pwmXguVtkD`), suggesting the table was garbage-collected or re-registered mid-pipeline. Likely a duckplyr interaction issue.

## PatientProfiles (1.4.5)

2 test failures. `filterInObservation()` generates SQL referencing column `"id_vgz"` which doesn't exist in the FROM clause. This appears to be an internal/generated column alias that doesn't get propagated to the outer query scope. A change in dbplyr's column aliasing/subquery scoping causes this.

## pool (1.0.4)

5 test failures + example error. `sql_escape_ident()` has no method for `Pool` objects. dbplyr's `make_table_path()` now calls `sql_escape_ident(con, ...)` where `con` is a raw Pool object rather than a checked-out DBI connection. pool needs to add a `sql_escape_ident.Pool` method, or dbplyr needs to handle non-DBI connection wrappers.

## RClickhouse (0.6.10)

2 test failures. `ClickhouseConnection` uses dbplyr's 1st edition interface, which is no longer supported. RClickhouse maintainer needs to update to 2nd edition.

## RPresto (1.4.8)

1 test failure. Test calls `dplyr::db_query_fields()` but that generic no longer has a method for `PrestoConnection`. The generic was likely removed or its dispatch path changed.

## SCDB (0.5.2)

16 test failures + example error + vignette error. Root cause: `as.sql()` was collapsed from a generic with an `as.sql.Id` method to a plain function that passed `DBI::Id` objects through unchanged, causing `escape.default()` to reject them. **Fixed** by restoring `DBI::Id` handling in `as.sql()`.
