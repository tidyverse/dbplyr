# Plan: Remove dbplyr 1e backend interface

## Background

dbplyr 2.0.0 (released June 2020) introduced a new backend API that moved generics from dplyr to dbplyr. The old 1e interface was deprecated in dbplyr 2.2.0 (September 2022). It's now time to remove the old interface completely.

## Current state

The codebase currently supports both 1e and 2e backends through:
1. `dbplyr_edition()` generic that backends implement (default returns `1L`)
2. `dbplyr_fallback()` function that checks edition and routes to appropriate implementation
3. Internal `dbplyr_*()` functions that call `dbplyr_fallback()`
4. Old dplyr generic methods (e.g., `sql_select.DBIConnection`) that forward to new generics
5. Direct edition checks (e.g., in `R/query-set-op.R:39`)

## Goal

Remove all 1e compatibility code and require all backends to use the 2e interface. Backends that haven't upgraded will receive a clear error message.

## Implementation steps

### 1. Create `check_2ed()` helper function

Add to `R/db.R`:

```r
check_2ed <- function(con, call = caller_env()) {
  edition <- dbplyr_edition(con)
  if (edition < 2) {
    class <- class(con)[[1]]
    cli_abort(
      c(
        "<{class}> uses dbplyr 1e interface which is no longer supported."
        i = "Please contact the maintainer of the package for a solution.",
      ),
      call = call
    )
  }
}
```

This ensures that dbplyr clearly errors for non-2e backends.
We'll keep `dbplyr_edition()` around to avoid breaking changes in existing backends.

### 2. Simplify internal dbplyr_* functions

Keep the internal `dbplyr_*()` wrapper functions but replace them with the implementation of the DBIConnection method. For example, if a `dbplyr_` function calls `dbplyr_fallback(con, "sql_translate_env")` then replace its body with the `sql_translate_env.DBIConnection`.

e.g.

```r
# Before:
dbplyr_sql_translation <- function(con) {
  dbplyr_fallback(con, "sql_translate_env")
}
#' @importFrom dplyr sql_translate_env
#' @export
sql_translate_env.DBIConnection <- function(con) {
  sql_translation(con)
}

# After:
dbplyr_sql_translation <- function(con) {
  check_2ed(con)
  sql_translation(con)
}

# Before
dbplyr_query_set_op <- function(con, ...) {
  dbplyr_fallback(con, "sql_set_op", ...)
}
#' @importFrom dplyr sql_set_op
#' @export
sql_set_op.DBIConnection <- function(con, x, y, method) {
  # dplyr::sql_set_op() doesn't have ...
  sql_query_set_op(con, x, y, method)
}

# After
#' @importFrom dplyr sql_set_op
dbplyr_query_set_op <- function(con, x, y, method) {
  check_2ed(con)
  # dplyr::sql_set_op() doesn't have ...
  sql_query_set_op(con, x, y, method)
}
```

Apply this pattern to all `dbplyr_*` functions.

**Note**: The `dbplyr_analyze`, `dbplyr_create_index`, etc. functions still call the old `db_*` generics (not the `sql_*` generics) because those methods handle both SQL generation and execution. The old `db_*.DBIConnection` wrapper methods will be removed in the next step.

### 4. Update direct edition checks

In `R/query-set-op.R` (lines 39-49), replace the edition check with just calling the internal wrapper:

```r
# Before:
if (dbplyr_edition(con) >= 2) {
  sql_query_set_op(...)
} else {
  # nocov start
  dbplyr_query_set_op(...)
  # nocov end
}

# After:
dbplyr_query_set_op(...)
```

The `check_2ed()` call in `dbplyr_query_set_op()` will handle the error case for 1e backends.

### 5. Search for other uses of `dbplyr_edition()`

Search the codebase for any other uses of `dbplyr_edition()` that might need updating.

### 6. Delete `dbplyr_fallback()` function

Remove the entire `dbplyr_fallback()` function from `R/db.R` (lines 220-237).

### 7. Remove old dplyr generic wrapper methods

Delete these methods that wrap the old dplyr generics (they're only for 1e compatibility):

From `R/db-sql.R`:
- `sql_translate_env.DBIConnection()`
- `sql_select.DBIConnection()`
- `sql_join.DBIConnection()`
- `sql_semi_join.DBIConnection()`
- `sql_set_op.DBIConnection()`
- `db_analyze.DBIConnection()`
- `db_create_index.DBIConnection()`
- `db_explain.DBIConnection()`
- `db_query_fields.DBIConnection()`
- `db_save_query.DBIConnection()`
- `sql_subquery.DBIConnection()`

From `R/db.R`:
- `db_desc.DBIConnection()`

### 8. Remove `@importFrom dplyr` statements

Remove these imports from the files since we're no longer using the dplyr generics:

From `R/db-sql.R`:
- `@importFrom dplyr sql_translate_env`
- `@importFrom dplyr sql_select`
- `@importFrom dplyr sql_join`
- `@importFrom dplyr sql_semi_join`
- `@importFrom dplyr sql_set_op`
- `@importFrom dplyr db_analyze`
- `@importFrom dplyr db_create_index`
- `@importFrom dplyr db_explain`
- `@importFrom dplyr db_query_fields`
- `@importFrom dplyr db_save_query`
- `@importFrom dplyr sql_subquery`

From `R/db.R`:
- `@importFrom dplyr db_desc`

Run `devtools::document()` to regenerate NAMESPACE

### 9. Update documentation

- Update `vignettes/backend-2.Rmd` to remove references to the timeline and compatibility layer
- Update `?db-sql` and `?db-misc` documentation to remove 1e deprecation notes
- Add a NEWS.md entry about removing 1e support

Run `devtools::document()` to regenerate NAMESPACE

### 10. Update tests

- Review and update tests that check edition behavior
- Remove tests for 1e compatibility
- Add tests that verify 1e backends error appropriately

## Testing strategy

1. Run existing tests to ensure 2e backends still work
2. Create a test backend with `dbplyr_edition.TestBackend1e <- function(con) 1L`
3. Verify it produces appropriate error messages
4. Test with real backend packages if possible (RPostgres, RSQLite, etc.)

## Migration path for backend maintainers

Backends that haven't upgraded to 2e will get a clear error message directing them to:
1. Implement `dbplyr_edition.YourConnection <- function(con) 2L`
2. Follow the instructions in `vignette("backend-2")`
3. Rename/restructure their methods according to the 2e API

## Risks and considerations

- **Breaking change**: Any backend that hasn't upgraded to 2e will stop working
- **Mitigation**: The 1e interface has been deprecated since dbplyr 2.2.0 (Sept 2022), giving maintainers 2+ years to upgrade
- **User impact**: Users with old backends will get clear error messages telling them to update their backend package

## Files to modify

- `R/db.R` - Add `check_2ed()`, update `dbplyr_edition.default()`, remove `dbplyr_fallback()`, remove old wrapper
- `R/db-sql.R` - Replace internal functions, remove old wrappers, remove imports, add `check_2ed()` calls
- `R/query-set-op.R` - Remove edition check
- `vignettes/backend-2.Rmd` - Update documentation
- `NEWS.md` - Add release note
- `tests/testthat/test-*.R` - Update tests
