# Build SQL strings with glue syntax

`glue_sql2()` is a SQL string builder that uses
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) syntax
with special type markers for safe SQL generation. It replaces the
deprecated
[`build_sql()`](https://dbplyr.tidyverse.org/dev/reference/build_sql.md)
function with a more explicit and type-safe approach.

Within the glue template, use type markers to specify how values should
be escaped and formatted:

- `.tbl` - A table identifier (e.g.,
  [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html), string, or
  [`in_schema()`](https://dbplyr.tidyverse.org/dev/reference/in_schema.md)).

- `.from` - A subquery or table identifier.

- `.name` - A name for an index or subquery (string or
  [`ident()`](https://dbplyr.tidyverse.org/dev/reference/ident.md)).

- `.col` - A column name or multiple columns (use `*` suffix for
  multiple).

- `.kw` - An SQL keyword that will be syntax-highlighted.

- `.val` - Any value to be escaped (vectors, dates, SQL, etc.).

The `*` suffix after `.col` or `.val` collapses the vector into a
comma-separated list.

If no type is specified, the value must be a string or scalar SQL and
won't be escaped or collapsed.

## Usage

``` r
glue_sql2(
  .con,
  ...,
  .sep = "",
  .envir = parent.frame(),
  .open = "{",
  .close = "}",
  .na = DBI::SQL("NULL"),
  .null = "",
  .comment = "#",
  .literal = FALSE,
  .trim = TRUE
)
```

## Arguments

- .con:

  A database connection.

- ...:

  SQL fragments to interpolate. These are evaluated in `.envir` and then
  combined according to their type markers.

- .sep:

  Separator to use between elements of `...`.

- .envir:

  Environment to evaluate `...` in.

- .open, .close:

  Opening and closing delimiters for interpolation.

- .na, .null:

  Values to use for `NA`/`NULL` values.

- .comment:

  Comment character to use.

- .literal:

  Whether to treat strings literally.

- .trim:

  Whether to trim whitespace.

## Value

An SQL string.

## Examples

``` r
con <- simulate_dbi()

tbl <- "my_table"
glue_sql2(con, "SELECT * FROM {.tbl tbl}")
#> <SQL> SELECT * FROM `my_table`

# Values are properly escaped
name <- "Robert'); DROP TABLE students;--"
glue_sql2(con, "INSERT INTO students (name) VALUES ({.val name})")
#> <SQL> INSERT INTO students (name) VALUES ('Robert''); DROP TABLE students;--')

# Multiple columns with collapse
cols <- c("name", "age", "grade")
glue_sql2(con, "SELECT {.col cols*} FROM students")
#> <SQL> SELECT `name`, `age`, `grade` FROM students
```
