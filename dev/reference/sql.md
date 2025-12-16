# SQL escaping.

These functions are critical when writing functions that translate R
functions to sql functions. Typically a conversion function should
escape all its inputs and return an sql object.

## Usage

``` r
sql(...)

is.sql(x)
```

## Arguments

- ...:

  Character vectors that will be combined into a single SQL vector.

- x:

  Object to check if it is an sql object.
