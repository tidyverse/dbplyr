# Escape/quote a string.

`escape()` requires you to provide a database connection to control the
details of escaping. `escape_ansi()` uses the SQL 92 ANSI standard.

## Usage

``` r
escape(x, parens = NA, collapse = " ", con = NULL)

escape_ansi(x, parens = NA, collapse = "")

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

  Database connection.

## Examples

``` r
# Doubles vs. integers
escape_ansi(1:5)
#> <SQL> (12345)
escape_ansi(c(1, 5.4))
#> <SQL> (1.05.4)

# String vs known sql vs. sql identifier
escape_ansi("X")
#> <SQL> 'X'
escape_ansi(sql("X"))
#> <SQL> X
escape_ansi(ident("X"))
#> <SQL> `X`

# Escaping is idempotent
escape_ansi("X")
#> <SQL> 'X'
escape_ansi(escape_ansi("X"))
#> <SQL> 'X'
escape_ansi(escape_ansi(escape_ansi("X")))
#> <SQL> 'X'
```
