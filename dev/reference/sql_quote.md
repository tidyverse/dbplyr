# Helper function for quoting sql elements.

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

  Single quoting character.

## Examples

``` r
sql_quote("abc", "'")
#> [1] "'abc'"
sql_quote("I've had a good day", "'")
#> [1] "'I''ve had a good day'"
sql_quote(c("abc", NA), "'")
#> [1] "'abc'" "NULL" 
```
