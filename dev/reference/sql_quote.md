# Helper function for quoting sql elements

If the closing quote character is present in the string, it will be
doubled or escaped with a backslash. `NA`s will be replaced with NULL.

## Usage

``` r
sql_quote(x, quote, backslash = FALSE)
```

## Arguments

- x:

  Character vector to escape. Strings wrapped in
  [`DBI::SQL()`](https://dbi.r-dbi.org/reference/SQL.html) are treated
  as already quoted.

- quote:

  Quote character. Either a length 1 character vector for symmetric
  quotes (e.g., `"'"` or `'"'`), or a length 2 character vector for
  asymmetric quotes (e.g., `c("[", "]")`).

- backslash:

  If `TRUE`, double backslashes and escape closing quote characters with
  a backslash instead of doubling them.

## Value

A vector of [sql](https://dbplyr.tidyverse.org/dev/reference/sql.md).

## Examples

``` r
sql_quote("abc", "'")
#> <SQL> 'abc'
sql_quote("I've had a good day", "'")
#> <SQL> 'I''ve had a good day'
sql_quote("I've had a good day", "'", backslash = TRUE)
#> <SQL> 'I\'ve had a good day'
sql_quote(c("abc", NA), "'")
#> <SQL> 'abc'
#> <SQL> NULL

sql_quote(c("abc", NA), c("[", "]"))
#> <SQL> [abc]
#> <SQL> NULL
```
