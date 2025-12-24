# Flag a character vector as SQL identifiers

`ident()` takes strings and turns them as database identifiers (e.g.
table or column names) quoting them using the identifier rules for your
database.
[`ident_q()`](https://dbplyr.tidyverse.org/dev/reference/ident_q.md)
does the same, but assumes the names have already been quoted,
preventing them from being quoted again.

These are generally for internal use only; if you need to supply an
table name that is qualified with schema or catalog, or has already been
quoted for some other reason, use
[`I()`](https://rdrr.io/r/base/AsIs.html).

## Usage

``` r
ident(...)

is.ident(x)
```

## Arguments

- ...:

  A character vector, or name-value pairs.

- x:

  An object.

## Examples

``` r
con <- simulate_dbi()

# SQL92 quotes strings with '
escape("x", con = con)
#> <SQL> 'x'

# And identifiers with "
ident("x")
#> <IDENT> x
escape(ident("x"), con = con)
#> <SQL> "x"

# You can supply multiple inputs
ident(a = "x", b = "y")
#> <IDENT> x
#> <IDENT> y
ident_q(a = "x", b = "y")
#> <IDENT> x
#> <IDENT> y
```
