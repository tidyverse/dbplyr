# SQL dialects

The dialect system allows multiple database connection classes to share
SQL generation code. A dialect object encapsulates the SQL syntax rules
for a particular database, independent of the connection mechanism.

- `sql_dialect()` returns the dialect for a connection. For connections
  that haven't implemented a dialect method, returns the connection
  itself for backward compatibility.

- `new_sql_dialect()` creates a new dialect object. This is primarily
  intended for dbplyr backend authors.

## Usage

``` r
sql_dialect(con)

new_sql_dialect(
  dialect,
  quote_identifier,
  has_window_clause = FALSE,
  has_table_alias_with_as = TRUE
)
```

## Arguments

- con:

  A database connection or dialect object.

- dialect:

  A string giving the dialect name (e.g., "postgres", "mysql").

- quote_identifier:

  A function that quotes identifiers. Should accept a character vector
  and return a [sql](https://dbplyr.tidyverse.org/dev/reference/sql.md)
  vector.

- has_window_clause:

  Does the backend support named window definitions (the `WINDOW`
  clause)?

- has_table_alias_with_as:

  Does the backend support using `AS` when aliasing a table in a
  subquery?

## Value

- `sql_dialect()` returns a dialect object (class `sql_dialect`) or the
  connection itself for backward compatibility.

- `new_sql_dialect()` returns a dialect object with class
  `c("sql_dialect_{name}", "sql_dialect")`.

## Examples

``` r
# Create a custom dialect
my_dialect <- new_sql_dialect(
  "custom",
  quote_identifier = function(x) sql_quote(x, "`"),
  has_window_clause = TRUE
)
class(my_dialect)
#> [1] "sql_dialect_custom" "sql_dialect"       
```
