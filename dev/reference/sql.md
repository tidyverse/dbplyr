# SQL escaping.

These functions are critical when writing functions that translate R
functions to sql functions. Typically a conversion function should
escape all its inputs and return an sql object.

## Usage

``` r
sql(...)

is.sql(x)

as.sql(x, con)
```

## Arguments

- ...:

  Character vectors that will be combined into a single SQL expression.

- x:

  Object to coerce

- con:

  Needed when `x` is directly supplied from the user so that schema
  specifications can be quoted using the correct identifiers.
