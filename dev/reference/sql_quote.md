# Helper function for quoting sql elements

If the quote character is present in the string, it will be doubled.
`NA`s will be replaced with NULL.

## Usage

``` r
sql_quote(x, quote)
```

## Arguments

- x:

  Character vector to escape.

- quote:

  Quote character. Either a length 1 character vector for symmetric
  quotes (e.g., `"'"` or `'"'`), or a length 2 character vector for
  asymmetric quotes (e.g., `c("[", "]")`).

## Value

A vector of .

## Examples

``` r
sql_quote("abc", "'")
#> <SQL> 'abc'
sql_quote("I've had a good day", "'")
#> <SQL> 'I''ve had a good day'
sql_quote(c("abc", NA), "'")
#> <SQL> 'abc'
#> <SQL> NULL

sql_quote(c("abc", NA), c("[", "]"))
#> <SQL> [abc]
#> <SQL> NULL
```
