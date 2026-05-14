# Revdep failure analysis

Remaining unresolved cluster after dbplyr-side fixes and downstream PRs (see
issue #1790).

## Cluster 2: SCDB's `id.tbl_dbi()` type dispatch

`remote_table()` no longer returns a `dbplyr_table_path`. After commit
`defbe959` (May 2026, "Ensure `remote_table()` returns user facing object"),
`remote_table()` wraps its result with `sql(unclass(table))` — see
`R/remote.R:53-61` and the documented return value in `R/remote.R:21-25`.
Inputs that aren't local or already SQL come back as a single `sql` /
`character` string, e.g. `` `main`.`mtcars` ``.

This breaks **SCDB**'s `id.tbl_dbi()` (`R/id.R:88-153`), and **diseasystore**
cascades from it.

### What was fixed

- **`as.sql.Id()`** (commit `21718a02`, "Restore `as.sql()` handling for
  `Id()`") — fixes SCDB's `update_snapshot.R:278` and `Logger.R:262`, which
  call `dbplyr::as.sql(id(...), con = ...)` on a `DBI::Id`. Now returns
  `sql(DBI::dbQuoteIdentifier(con, x))`.
- **`remote_table()` inlined into `build_sql()`** — SCDB's
  `update_snapshot.R:300-301` interpolates `dbplyr::remote_table(snapshot)`
  into a `build_sql(...)` call. Because the return is now a `sql()` object,
  `escape()` passes it straight through, producing valid SQL.

### What's still broken

SCDB's `id.tbl_dbi()` branches on the return type of `remote_table()`:

```r
table_ident <- dbplyr::remote_table(db_table)

if (inherits(table_ident, "dbplyr_table_path")) {  # dbplyr >= 2.5.0
  components <- dbplyr::table_path_components(table_ident, table_conn)[[1]]
  ...
} else {                                            # legacy: assume DBI::Id
  table_ident <- table_ident |> unclass() |> purrr::discard(is.na)
  catalog <- purrr::pluck(table_ident, "catalog")
  schema  <- purrr::pluck(table_ident, "schema")
  table   <- purrr::pluck(table_ident, "table")
}

matches <- get_tables(...) |> dplyr::filter(.data$table == !!table)
```

With current dbplyr the result is `<sql/character>`, so `inherits(...,
"dbplyr_table_path")` is `FALSE` and it falls into the legacy branch. The
character vector has no `"catalog"`/`"schema"`/`"table"` element, so all three
are `NULL`, and the subsequent `filter(.data$table == !!NULL)` aborts with
`..1 must be of size N or 1, not size 0` — which is exactly the failure in
problems.md (SCDB tests/examples/vignettes; diseasystore tests cascade with
the same `dplyr_internal_error` trace from `mask$eval_all_filter`).

### Suggested fix in SCDB

`table_path_components()` already accepts a `sql`/`character` input (verified:
`table_path_components(sql("`main`.`mtcars`"), con)` returns
`list(c("main", "mtcars"))`). Drop the type branching entirely:

```r
table_ident <- dbplyr::remote_table(db_table)
components  <- dbplyr::table_path_components(table_ident, table_conn)[[1]]
components  <- rev(components)
table   <- components[[1]]
schema  <- components[[2]] %||% NULL
catalog <- components[[3]] %||% NULL
```

This works against both old (`dbplyr_table_path`) and new (`sql`) return
types, so SCDB doesn't need a hard dbplyr version bump.

### diseasystore

The truncated trace in problems.md ends in `mask$eval_all_filter` →
`dplyr_internal_error`, matching SCDB's cascade. diseasystore depends on
`SCDB::update_snapshot()`, so fixing SCDB should resolve diseasystore's test
failures. (The vignette's "Lock not released within 30 minutes" looks
secondary — likely the test process holding a file lock after the earlier
abort. Worth re-checking once SCDB is fixed.)
