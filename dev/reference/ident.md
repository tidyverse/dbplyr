# Flag a character vector as SQL identifiers

`ident()` marks strings as database identifiers (e.g. table or column
names) quoting them using the identifier rules for your database. It is
used primarily in
[`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
to label variables as identifiers; use elsewhere should be regarded with
suspicion.

`ident()` is for internal use only; if you need to supply an table name
that is qualified with schema or catalog use
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
con <- dialect_ansi()

# SQL92 quotes strings with '
escape("x", con = con)
#> <SQL> 'x'

# And identifiers with "
escape(ident("x"), con = con)
#> <SQL> "x"
```
