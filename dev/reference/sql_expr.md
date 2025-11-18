# Generate SQL from R expressions

Low-level building block for generating SQL from R expressions. Strings
are escaped; names become bare SQL identifiers. User infix functions
have `%` stripped.

## Usage

``` r
sql_expr(x, con = sql_current_con())

sql_call2(.fn, ..., con = sql_current_con())
```

## Arguments

- x:

  A quasiquoted expression

- con:

  Connection to use for escaping. Will be set automatically when called
  from a function translation.

- .fn:

  Function name (as string, call, or symbol)

- ...:

  Arguments to function

## Details

Using `sql_expr()` in package will require use of
[`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html) to
avoid `R CMD check` NOTES. This is a small amount of additional pain,
which I think is worthwhile because it leads to more readable
translation code.

## Examples

``` r
con <- simulate_dbi() # not necessary when writing translations

sql_expr(f(x + 1), con = con)
#> <SQL> F(x + 1.0)
sql_expr(f("x", "y"), con = con)
#> <SQL> F('x', 'y')
sql_expr(f(x, y), con = con)
#> <SQL> F(x, y)

x <- ident("x")
sql_expr(f(!!x, y), con = con)
#> <SQL> F(`x`, y)

sql_expr(cast("x" %as% DECIMAL), con = con)
#> <SQL> CAST('x' AS DECIMAL)
sql_expr(round(x) %::% numeric, con = con)
#> <SQL> ROUND(x) :: numeric

sql_call2("+", quote(x), 1, con = con)
#> <SQL> x + 1.0
sql_call2("+", "x", 1, con = con)
#> <SQL> 'x' + 1.0
```
