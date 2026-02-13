# Escape/quote a value

`escape()` turns R values into SQL literals. It implements double
dispatch via two sets of generics: first `escape()` dispatches on the
class of `x`, then that method calls `sql_escape_ident()`,
`sql_escape_logical()`, etc, which dispatch on `con`.

These generics translate individual values into SQL. The core generics
are
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
and
[`DBI::dbQuoteString()`](https://dbi.r-dbi.org/reference/dbQuoteString.html)
for quoting identifiers and strings, but dbplyr needs additional tools
for inserting logical, date, date-time, and raw values into queries.

## Usage

``` r
escape(x, parens = NA, collapse = " ", con = NULL)

sql_escape_ident(con, x)

sql_escape_logical(con, x)

sql_escape_date(con, x)

sql_escape_datetime(con, x)

sql_escape_string(con, x)

sql_escape_raw(con, x)

sql_vector(x, parens = NA, collapse = " ", con = NULL)
```

## Arguments

- x:

  An object to escape. Existing sql vectors will be left as is,
  character vectors are escaped with single quotes, numeric vectors have
  trailing `.0` added if they're whole numbers, identifiers are escaped
  with double quotes.

- parens, collapse:

  Controls behaviour when multiple values are supplied. `parens` should
  be a logical flag, or if `NA`, will wrap in parens if length \> 1.

  Default behaviour: lists are always wrapped in parens and separated by
  commas, identifiers are separated by commas and never wrapped, atomic
  vectors are separated by spaces and wrapped in parens if needed.

- con:

  A
  [sql_dialect](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
  object or database connection. Connections are supported for backward
  compatibility.

## Value

A [sql](https://dbplyr.tidyverse.org/dev/reference/sql.md) vector.

## See also

Other generic:
[`db-sql`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
[`db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md),
[`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)

## Examples

``` r
con <- dialect_ansi()

# Doubles vs. integers
escape(1:5, con = con)
#> <SQL> (1, 2, 3, 4, 5)
escape(c(1, 5.4), con = con)
#> <SQL> (1.0, 5.4)

# String vs known sql vs. sql identifier
escape("X", con = con)
#> <SQL> 'X'
escape(sql("X"), con = con)
#> <SQL> X
escape(ident("X"), con = con)
#> <SQL> "X"

# Escaping is idempotent
escape("X", con = con)
#> <SQL> 'X'
escape(escape("X", con = con), con = con)
#> <SQL> 'X'

# Database specific generics
sql_escape_logical(con, c(TRUE, FALSE, NA))
#> <SQL> TRUE
#> <SQL> FALSE
#> <SQL> NULL
sql_escape_date(con, Sys.Date())
#> <SQL> '2026-02-13'
sql_escape_date(con, Sys.time())
#> <SQL> '2026-02-13 14:15:33.993919'
sql_escape_raw(con, charToRaw("hi"))
#> <SQL> X'6869'
```
