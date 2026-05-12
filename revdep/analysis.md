# Revdep failure analysis

10 downstream packages investigated. Most failures cluster on three dbplyr
changes; the table at the bottom maps each package to its root cause.

## Cluster 1: `sql_escape_string.default` no longer passes `SQL` through (PR #1701, commit `3e7389cd`)

`sql_escape_string.default` was simplified from `DBI::dbQuoteString(con, x)`
(which is idempotent for `DBI::SQL` inputs) to `sql(sql_quote(x, "'"))`, which
unconditionally wraps the value in single quotes. Downstream code that calls
`dbplyr::escape(DBI::dbQuoteIdentifier(con, name), con = con)` now produces
double-quoted output like `` '`name`' `` instead of `` `name` ``.

Hits: **dm**, **rolap**, **when**.

- **rolap** & **when**: `dm::copy_dm_to()` uses this pattern in
  `ddl_get_index_defs()` / `ddl_quote_table_names()`. `DBI::dbUnquoteIdentifier()`
  then rejects the double-quoted output, and SQLite stores tables under literal
  names like `` `date` ``.
- **dm**: same regression, plus a separate issue (see Cluster 4 below).

Suggested fix in dbplyr (`R/escape.R:167`):

```r
sql_escape_string.default <- function(con, x) {
  if (inherits(x, "SQL")) return(sql(unclass(x)))
  sql(sql_quote(x, "'"))
}
```

or revert to `sql(DBI::dbQuoteString(con, x))`.

## Cluster 2: `escape.dbplyr_table_path()` removed (commit `c1f77a6b`)

`remote_table()` returns a `dbplyr_table_path` (which inherits from
`character`). Without the dedicated escape method, `build_sql()` falls through
to `escape.character()` and renders the table name as a string literal
(`'dbplyr_tmp_XXX'`). SQLite reports "no such table"; DuckDB reports
`Cannot extract field 'checksum' from expression '...'`.

Hits: **SCDB**, **diseasystore** (cascades from SCDB).

SCDB's `update_snapshot()` interpolates `dbplyr::remote_table(snapshot)` into
`dbplyr::build_sql(...)` — a documented public API.

Suggested fix in dbplyr: revert the `R/table-name.R` / `NAMESPACE` hunks of
`c1f77a6b` to re-introduce `escape.dbplyr_table_path()`.

## Cluster 3: tbl-construction refactor (commit `133f1a14`, PR #1680)

`tbl_sql()`/`tbl_lazy()` were refactored so the lazy tbl carries a top-level
`con` field; `src$con` is kept only as a backward-compat shim.

- **mlr3db**: `DataBackendDplyr$.reconnect()` swaps `private$.data$src$con <-
  con`, which now only updates the stale shim. Top-level `x$con` still
  references the closed connection, so queries hit `bad_weak_ptr`. **Fix in
  mlr3db**: also assign `private$.data$con <- con` (dbplyr never promised that
  mutating `src$con` was supported).
- **osdc**: duckplyr's `as_tbl()` attaches a finalizer-bearing environment as
  `attr(., "duckplyr_scope_guard")` to keep its temp view alive. Somewhere
  along the new tbl construction chain that attribute is not propagated, so GC
  drops the view before `collect()` runs, leading to `Table as_tbl_duckplyr_XXX
  does not exist`. Exact drop point not pinpointed — needs an
  attribute-propagation trace through `new_tbl_lazy()` / `make_tbl()` and the
  intermediate verbs. **Fix likely in dbplyr.**

## Cluster 4: individual regressions

- **pool**: `sql_escape_ident()` was rewritten in PR #1760 to dispatch via
  `sql_dialect(con)` and lost its `default` method. Pool (R6, not a
  `DBIConnection`) implements `dbQuoteIdentifier` via S4 but no longer matches
  any `sql_escape_ident` method, so `UseMethod` aborts. **Fix in dbplyr**:
  restore `sql_escape_ident.default <- function(con, x) sql(DBI::dbQuoteIdentifier(con, x))`.
  Audit sibling generics (`sql_escape_string`, `sql_escape_logical`,
  `sql_escape_date`, `sql_escape_datetime`, `sql_escape_raw`).
- **healthdb**: in PR #1691 the `%in%` guard was narrowed from `is.sql(table)`
  to `is.ident(table)`, so a pre-parenthesised `sql()` RHS now gets wrapped in
  an extra set of parens producing `IN ((...))`, which SQLite rejects as a row
  value. healthdb's `identify_rows_sql()` uses
  `escape_ansi(vals, collapse = ",", parens = TRUE)` inside `sql()`. **Fix in
  dbplyr** (`R/backend-.R:164-175`): broaden the guard back to `is.sql(table)`.
- **PatientProfiles**: regression from PR #1761 ("Inline `filter()` after a
  left/inner join"). `add_select()` mutates
  `lazy_multi_join_query$vars` but doesn't update `$where`, so a
  `join |> filter(renamed_col) |> select(!renamed_col)` pattern leaks the
  dropped/renamed column into the WHERE clause as an unqualified symbol. **Fix
  in dbplyr** (`R/verb-select.R`): also rewrite/refresh `lazy_query$where`, or
  fall through to `lazy_select_query()` when any symbol in `where` would be
  dropped by the `select`.
- **dm** also has an independent issue: `tbl_sum.tbl_sql` was deleted in PR
  #1679. dm's `tests/testthat/setup.R` calls
  `local_mocked_bindings(tbl_sum.tbl_sql = ..., .package = "dbplyr")` and now
  aborts. This is intentional dbplyr removal — **fix in dm**.

## Summary table

| Package         | Cluster                                    | Suggested fix location |
|-----------------|--------------------------------------------|------------------------|
| diseasystore    | 2 (escape.dbplyr_table_path)               | dbplyr                 |
| dm              | 1 (sql_escape_string) + tbl_sum removal    | dbplyr + dm            |
| healthdb        | 4 (%in% guard)                             | dbplyr                 |
| mlr3db          | 3 (tbl refactor)                           | mlr3db                 |
| osdc            | 3 (tbl refactor — attribute propagation)   | dbplyr                 |
| PatientProfiles | 4 (filter-inlined join + select)           | dbplyr                 |
| pool            | 4 (sql_escape_ident default removed)       | dbplyr                 |
| rolap           | 1 (sql_escape_string)                      | dbplyr                 |
| SCDB            | 2 (escape.dbplyr_table_path)               | dbplyr                 |
| when            | 1 (sql_escape_string)                      | dbplyr                 |

9 of 10 failures point to a dbplyr-side fix; only mlr3db is a clean downstream
issue, and dm has one downstream component on top of the dbplyr regression.
