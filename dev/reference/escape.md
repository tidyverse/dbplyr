# Escape/quote a string.

`escape()` requires you to provide a database connection to control the
details of escaping.

## Usage

``` r
escape(x, parens = NA, collapse = " ", con = NULL)

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
con <- simulate_dbi()

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
```
