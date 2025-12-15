# Partially evaluate an expression.

This function partially evaluates an expression, using information from
the tbl to determine whether names refer to local expressions or remote
variables. This simplifies SQL translation because expressions don't
need to carry around their environment - all relevant information is
incorporated into the expression.

## Usage

``` r
partial_eval(call, data, env = caller_env(), error_call = caller_env())
```

## Arguments

- call:

  an unevaluated expression, as produced by
  [`quote()`](https://rdrr.io/r/base/substitute.html)

- data:

  A lazy data frame backed by a database query.

- env:

  environment in which to search for local values

## Symbol substitution

`partial_eval()` needs to guess if you're referring to a variable on the
server (remote), or in the current environment (local). It's not
possible to do this 100% perfectly. `partial_eval()` uses the following
heuristic:

- If the tbl variables are known, and the symbol matches a tbl variable,
  then remote.

- If the symbol is defined locally, local.

- Otherwise, remote.

You can override the guesses using
[`local()`](https://rdrr.io/r/base/eval.html) and `remote()` to force
computation, by using the `.data` and `.env` pronouns of tidy
evaluation, or by using dbplyr's own `.sql` pronoun.

## Examples

``` r
lf <- lazy_frame(year = 1980, id = 1)
partial_eval(quote(year > 1980), data = lf)
#> year > 1980

ids <- c("ansonca01", "forceda01", "mathebo01")
partial_eval(quote(id %in% ids), lf)
#> id %in% c("ansonca01", "forceda01", "mathebo01")

# cf.
partial_eval(quote(id == .data$id), lf)
#> id == id

# You can use local() or .env to disambiguate between local and remote
# variables: otherwise remote is always preferred
year <- 1980
partial_eval(quote(year > year), lf)
#> year > year
partial_eval(quote(year > local(year)), lf)
#> year > 1980
partial_eval(quote(year > .env$year), lf)
#> year > 1980

# Functions are always assumed to be remote. Use local to force evaluation
# in R.
f <- function(x) x + 1
partial_eval(quote(year > f(1980)), lf)
#> year > f(1980)
partial_eval(quote(year > local(f(1980))), lf)
#> year > 1981

# You can use `.sql` to make it clear that the function comes from SQL,
# and inside a package, reduce the number of globalVariables() directives
# needed
partial_eval(quote(.sql$EXTRACT_YEAR(year)), lf)
#> EXTRACT_YEAR(year)
```
