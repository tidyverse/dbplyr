# Build a SQL string.

**\[superseded\]** `build_sql()` is superseded in favor of
[`sql_glue2()`](https://dbplyr.tidyverse.org/dev/reference/sql_glue.md).

This is a convenience function that should prevent sql injection attacks
(which in the context of dplyr are most likely to be accidental not
deliberate) by automatically escaping all expressions in the input,
while treating bare strings as sql. This is unlikely to prevent any
serious attack, but should make it unlikely that you produce invalid
sql.

This function should be used only when generating `SELECT` clauses,
other high level queries, or for other syntax that has no R equivalent.
For individual function translations, prefer
[`sql_expr()`](https://dbplyr.tidyverse.org/dev/reference/sql_expr.md).

## Usage

``` r
build_sql(..., .env = parent.frame(), con = sql_current_con())
```

## Arguments

- ...:

  input to convert to SQL. Use
  [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) to
  preserve user input as is (dangerous), and
  [`ident()`](https://dbplyr.tidyverse.org/dev/reference/ident.md) to
  label user input as sql identifiers (safe)

- .env:

  the environment in which to evaluate the arguments. Should not be
  needed in typical use.

- con:

  database connection; used to select correct quoting characters.

## Examples

``` r
con <- simulate_dbi()

# Old:
build_sql("SELECT * FROM ", ident("table"), con = con)
#> Warning: `build_sql()` was deprecated in dbplyr 2.6.0.
#> ℹ Please use `sql_glue2()` instead.
#> <SQL> SELECT * FROM "table"
# New:
sql_glue2(con, "SELECT * FROM {.tbl 'table'}")
#> <SQL> SELECT * FROM "table"

# Old:
name <- "Robert"
build_sql("INSERT INTO students (name) VALUES (", name, ")", con = con)
#> <SQL> INSERT INTO students (name) VALUES ('Robert')
# New:
sql_glue2(con, "INSERT INTO students (name) VALUES ({name})")
#> <SQL> INSERT INTO students (name) VALUES ('Robert')
```
