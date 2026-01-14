# Build SQL strings with glue syntax

`sql_glue()` and `sql_glue2()` are designed to help dbplyr extenders
generate custom SQL. They differ only in whether or not they require a
connection. `sql_glue()` retrieves the ambient connection, making it
suitable for use inside
[`sql_translation()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
methods; `sql_glue2()` requires a connection, making it suitable for use
inside all other `sql_` methods.

As the name suggests, these functions use glue syntax to make it easy to
mix fixed SQL with varying user inputs. The default glue syntax, `{x}`,
will escape `x` using the database connection. If there are multiple
values in `x`, they'll be collapsed into a single string with `,`. If
you want them to be wrapped in `()`, use a `*` suffix, e.g. `{x*}`.

You can also use type markers to control how the value is treated:

- `{.sql x}`: `x` is literal SQL that should be interpolated as is,
  without additional escaping. `x` must be a string.

- `{.tbl x}`: `x` is a table identifier like a string,
  [`I()`](https://rdrr.io/r/base/AsIs.html), or one of the older forms
  like [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html) or
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md).

- `{.id x}`: `x` is a generic identifier, e.g. for a column or index.
  `x` must be a character vector

## Usage

``` r
sql_glue(sql, envir = parent.frame())

sql_glue2(con, sql, envir = parent.frame())
```

## Arguments

- sql:

  A string to interpolate.

- envir:

  Environment to evaluate `sql` in.

- con:

  A
  [sql_dialect](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
  object or database connection. Connections are supported for backward
  compatibility.

## Value

An SQL string.

## Examples

``` r
con <- simulate_dbi()

tbl <- "my_table"
sql_glue2(con, "SELECT * FROM {.tbl tbl}")
#> <SQL> SELECT * FROM "my_table"

# Values are properly escaped
name <- "Robert'); DROP TABLE students;--"
sql_glue2(con, "INSERT INTO students (name) VALUES ({name})")
#> <SQL> INSERT INTO students (name) VALUES ('Robert''); DROP TABLE students;--')

# Control wrapping with *
x <- c("name", "age", "grade")
sql_glue2(con, "SELECT {.id x} FROM students")
#> <SQL> SELECT "name", "age", "grade" FROM students
sql_glue2(con, "SELECT * WHERE variable IN {x*}")
#> <SQL> SELECT * WHERE variable IN ('name', 'age', 'grade')
```
