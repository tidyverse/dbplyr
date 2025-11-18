# Build a SQL string.

This is a convenience function that should prevent sql injection attacks
(which in the context of dplyr are most likely to be accidental not
deliberate) by automatically escaping all expressions in the input,
while treating bare strings as sql. This is unlikely to prevent any
serious attack, but should make it unlikely that you produce invalid
sql.

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

## Details

This function should be used only when generating `SELECT` clauses,
other high level queries, or for other syntax that has no R equivalent.
For individual function translations, prefer
[`sql_expr()`](https://dbplyr.tidyverse.org/dev/reference/sql_expr.md).

## Examples

``` r
con <- simulate_dbi()
build_sql("SELECT * FROM TABLE", con = con)
#> <SQL> SELECT * FROM TABLE
x <- "TABLE"
build_sql("SELECT * FROM ", x, con = con)
#> <SQL> SELECT * FROM 'TABLE'
build_sql("SELECT * FROM ", ident(x), con = con)
#> <SQL> SELECT * FROM `TABLE`
build_sql("SELECT * FROM ", sql(x), con = con)
#> <SQL> SELECT * FROM TABLE

# http://xkcd.com/327/
name <- "Robert'); DROP TABLE Students;--"
build_sql("INSERT INTO Students (Name) VALUES (", name, ")", con = con)
#> <SQL> INSERT INTO Students (Name) VALUES ('Robert''); DROP TABLE Students;--')
```
