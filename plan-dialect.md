## Goal

Implement a `sql_dialect()` system to reduce method duplication across database backends by:
1. Creating a dialect object that acts as an (optional) intermediary in between the database connection and SQL translation.
2. Allowing multiple connection classes to share a single dialect.
3. Maintaining backward compatibility for user-facing APIs

## Current Problems

- PostgreSQL has both `PostgreSQL` and `PqConnection` classes
- MySQL has `MySQL`, `MySQLConnection`, and `MariaDBConnection`
- Each requires duplicate method definitions:
  ```r
  sql_translation.PqConnection <- function(...) { ... }
  sql_translation.PostgreSQL <- sql_translation.PqConnection
  ```
- ~60 alias definitions across all backends

There's no easy way to support backends that talk to multiple different datasets (e.g. odbc, adbc).

## Proposed Architecture

### 1. SQL Dialect Object Structure

Introduce new dialect object used for dispatch:

```r
new_sql_dialect <- function(dialect, quote_identifier, supports_window_clause = FALSE, supports_table_alias_with_as = FALSE) {
  check_string(dialect)
  check_function(quote_identifier)
  check_bool(supports_window_clause)
  check_bool(supports_table_alias_with_as)

  structure(
    list(
      quote_identifier = quote_identifier,
      supports_window_clause = supports_window_clause,
      supports_table_alias_with_as = supports_table_alias_with_as
    ),
    class = c(paste0("sql_dialect_", dialect), "sql_dialect"))
}
```

**Key decisions**:
- Translations remain separate in `sql_translation()` methods
- Inheritance handled at translation level (not in dialect object)
- Dialect now stores support booleans rather than using generics.
  (This change only affects the dittodb package.)
  

### 2. Dispatch Mechanism

Use **iterated dispatch**: connection → dialect, then dialect → method.

```r
#' Get SQL dialect for a connection
#' @export
sql_dialect <- function(con) {
  UseMethod("sql_dialect")
}

#' @export
sql_dialect.DBIConnection <- function(con) {
  # For backward compatibility
  con
}
```

Then each backend defines its dialect:

```r
# In backend-postgres.R
#' @export
sql_dialect.PqConnection <- function(con) {
  dialect_postgres()
}

#' @export
sql_dialect.PostgreSQL <- function(con) {
  dialect_postgres()
}

#' 
#' @export
dialect_postgres <- function(con = NULL) {
  new_sql_dialect("postgres",
    quote_identifier = function(x) sql_quote(x, '"'),
    supports_window_clause = TRUE,
    supports_table_alias_with_as = TRUE
  )
}
```

### 3. SQL generation generics call `sql_dialect()`

All SQL generation generics first call `sql_dialect()` and then dispatch on that. Note that due to the limitations of `UseMethod()` this requires a special helper.

```R
#' @export
#' @rdname db-sql
sql_translation <- function(con) {
  dialect <- sql_dialect(con)
  return(sql_translation_(dialect))

  # never reached; needed for roxygen2
  UseMethod("sql_translation")
}
sql_translation_ <- function(dialect) {
  UseMethod("sql_translation")
}
```

This extra layer in conjunction with `sql_dialect.DBIConnection` above, means that `sql_dialect()` is opt-in and existing extensions will continue to work.

**Which generics need this pattern?**

As a general rule:
- **`sql_*` functions** work with dialect objects. They use iterated dispatch (connection → dialect) for backward compatibility.
- **`db_*` functions** work with connection objects. They perform database operations that require a real connection.

The following SQL generation generics should use iterated dispatch:
- `sql_translation()` - returns translation environment
- `sql_expr_matches()` - generates SQL for equality with NULL matching
- `sql_query_*()` - all query generation functions
- `sql_table_*()` - table manipulation functions
- `sql_escape_*()` - escaping functions

The `db_*` functions (like `db_connection_describe()`, `db_col_types()`) continue to dispatch directly on the connection object.

The support boolean generics (`supports_window_clause()` and `db_supports_table_alias_with_as()`) will be replaced by accessing dialect fields directly (see Section 7 below).

### 4. SQL escaping with dialect objects

SQL generation methods use `sql_glue2()` and other functions that call `sql_escape_ident()`, `sql_escape_string()`, etc. These escape functions need to work with dialect objects.

**Identifier escaping:**

Add a method for `sql_escape_ident()` that dispatches on dialect objects and uses the stored `quote_identifier` function:

```r
#' @export
sql_escape_ident.sql_dialect <- function(con, x) {
  con$quote_identifier(x)
}
```

**String escaping:**

String escaping uses `sql_quote()` with single quotes, which is consistent across most databases. The default method works for dialect objects:

```r
#' @export
sql_escape_string.sql_dialect <- function(con, x) {
 sql(sql_quote(x, "'"))
}
```

**Other escape functions:**

Add `sql_dialect` methods for other escape functions (`sql_escape_logical`, `sql_escape_date`, `sql_escape_datetime`, `sql_escape_raw`) that provide sensible defaults. Backends can override these by adding methods on their specific dialect class (e.g., `sql_escape_date.sql_dialect_postgres`).

**`sql_current_con()` returns a dialect:**

The `sql_glue()` function (used inside translation functions) calls `sql_current_con()` to get the connection for escaping. With the dialect system, `sql_current_con()` will return the dialect object, not the original connection. This works because the escape functions now dispatch on dialect objects.

```r
# Inside sql_translation.sql_dialect_postgres, sql_glue() works because:
# 1. sql_glue() calls sql_current_con() which returns the dialect
# 2. Escaping dispatches on the dialect via sql_escape_ident.sql_dialect
str_locate = function(string, pattern) {
  sql_glue("STRPOS({string}, {pattern})")
}
```

**`db_connection_describe()` remains connection-based:**

The `db_connection_describe()` function retrieves actual connection metadata (host, username, database name) and must continue to dispatch on the connection object, not the dialect.

### 5. Update existing methods

Next we need to update each `sql_*` method to instead dispatch on the dialect.

**Important**: Backend implementers write methods directly on the dialect class (e.g., `sql_expr_matches.sql_dialect_postgres`). The iterated dispatch wrapper is only needed in the user-facing generic (handled in Section 3).

```R
# Before
sql_expr_matches.PqConnection <- function(con, x, y, ...) {
  # https://www.postgresql.org/docs/current/functions-comparison.html
  glue_sql2(con, "{x} IS NOT DISTINCT FROM {y}")
}
sql_expr_matches.PostgreSQL <- sql_expr_matches.PqConnection

# After
sql_expr_matches.sql_dialect_postgres <- function(con, x, y, ...) {
  # https://www.postgresql.org/docs/current/functions-comparison.html
  glue_sql2(con, "{x} IS NOT DISTINCT FROM {y}")
}
```

Note: The first argument is still named `con` for consistency, but it will receive the dialect object when called through the iterated dispatch system.

### 6. Replace "simulate_" with "dialect_" functions

Old simulate functions should be deprecated in favor of exporting the new `dialect_` functions.

```r
simulate_postgres <- function() {
  lifecycle::deprecate_warn("2.6.0", "simulate_postgres()", "dialect_postgres()")
  dialect_postgres()
}
```

The deprecated `simulate_*()` functions now return dialect objects. Existing tests will continue to work because the SQL generation and escaping functions are updated to work with dialect objects (see Sections 3 and 4).

### 7. Accessing dialect properties

The dialect object stores support booleans as fields rather than using generics. Code that needs to check these properties should access them directly from the dialect.

```r
# Old approach - using generics
if (supports_window_clause(con)) {
  # generate WINDOW clause
}

# New approach - accessing dialect fields
dialect <- sql_dialect(con)
if (dialect$supports_window_clause) {
  # generate WINDOW clause
}
```

The old `supports_window_clause()` and `db_supports_table_alias_with_as()` can then be deleted as they are not used outside of dbplyr.

This maintains the existing API while using the new dialect system internally. Eventually, internal dbplyr code can migrate to accessing dialect fields directly for efficiency.

### 8. Supporting ODBC and ADBC

A key benefit of the dialect system is supporting backends that connect to multiple database types (like ODBC and ADBC). These backends can inspect the connection and return the appropriate dialect:

```r
#' @export
sql_dialect.OdbcConnection <- function(con) {
  # Dispatch based on the actual database driver
  dbms <- con@info$dbms.name

  switch(dbms,
    PostgreSQL = dialect_postgres(),
    MySQL = dialect_mysql(),
    "Microsoft SQL Server" = dialect_mssql(),
    SQLite = dialect_sqlite(),
    # Add more as needed
    dialect_odbc_generic()  # fallback for unknown databases
  )
}

# Fallback dialect for unknown ODBC databases
dialect_odbc_generic <- function() {
  new_sql_dialect("odbc_generic")
}
```

This allows ODBC connections to automatically use the correct SQL dialect for the underlying database, eliminating the need for separate backend implementations for each ODBC driver.

### 9. Migration strategy

Backends can migrate incrementally:

1. **Add dialect definition**: Create `sql_dialect.ClassName()` method and `dialect_name()` helper
2. **Migrate methods one at a time**: Update `sql_*.ClassName()` methods to `sql_*.sql_dialect_name()`
3. **Delete aliases**: Once a method is migrated, delete the duplicate definitions for other connection classes
4. **Delete old support methods**: Once fully migrated, remove old `supports_*()` method definitions

The `sql_dialect.DBIConnection()` default ensures backward compatibility - existing backends that haven't migrated yet will continue to work because their connection object will be passed through unchanged.

**Migration can happen one backend at a time**. There's no need to migrate all backends simultaneously. Mixed states are fully supported during the transition period.

### 10. Exports and documentation

**What to export:**
- `sql_dialect()` - user-facing generic (already exported via `@export` in db-sql.R)
- `new_sql_dialect()` - constructor for extension authors
- `dialect_*()` - dialect helpers for each backend (e.g., `dialect_postgres()`, `dialect_mysql()`)

**What NOT to export:**
- `sql_*_()` helpers (internal use only, underscore indicates private)
- `sql_dialect_*` classes (implicit through S3 dispatch, no need for explicit export)

**Documentation updates needed:**
1. Update `?db-sql` help topic to explain the dialect system and iterated dispatch
2. Add new `?sql_dialect` help topic documenting the generic and how to implement it
3. Add `?new_sql_dialect` topic for extension authors
4. Update `vignette("new-backend")` to show the dialect approach
5. Add dialect functions to `_pkgdown.yml` reference index

### 11. Testing approach

**Updating existing tests:**

Existing tests using `simulate_*()` should be updated to use `dialect_*()` functions:

```r
# Before
test_that("postgres translation works", {

  con <- simulate_postgres()

  expect_equal(
    sql_expr_matches(con, "x", "y"),
    sql("x IS NOT DISTINCT FROM y")
  )
})

# After
test_that("postgres translation works", {

  con <- dialect_postgres()

  expect_equal(
    sql_expr_matches(con, "x", "y"),
    sql("x IS NOT DISTINCT FROM y")
  )
})
```
**Testing the dialect system itself:**
- Add tests verifying that `sql_dialect()` returns the correct dialect for each connection type
- Test that dialect properties (like `supports_window_clause`) are correctly set
- Test ODBC/ADBC dynamic dialect selection 
