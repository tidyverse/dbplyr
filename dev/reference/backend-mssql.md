# SQL Server backend

This backend supports Microsoft SQL Server, typically accessed via odbc.
Use `dialect_mssql()` with
[`lazy_frame()`](https://dbplyr.tidyverse.org/dev/reference/tbl_lazy.md)
to see simulated SQL without connecting to a live database.

Key differences for this backend are:

- `SELECT` uses `TOP` not `LIMIT`

- Automatically prefixes `#` to create temporary tables. Add the prefix
  yourself to avoid the message.

- String basics: [`paste()`](https://rdrr.io/r/base/paste.html),
  [`substr()`](https://rdrr.io/r/base/substr.html),
  [`nchar()`](https://rdrr.io/r/base/nchar.html)

- Custom types for `as.*` functions

- Lubridate extraction functions, `year()`, `month()`, `day()` etc

- Semi-automated bit \<-\> boolean translation (see below)

- stringr functions `str_detect()`, `str_starts()`, `str_ends()` with
  `fixed()` patterns work on all versions; regular expression patterns
  require SQL Server 2025+ (version 17.0)

- stringr functions `str_replace()`, `str_replace_all()`,
  `str_remove()`, `str_remove_all()`, `str_extract()`, and `str_count()`
  require SQL Server 2025+ (version 17.0)

See
[`vignette("translation-function")`](https://dbplyr.tidyverse.org/dev/articles/translation-function.md)
and
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
for details of overall translation technology.

## Usage

``` r
dialect_mssql(version = "15.0")

simulate_mssql(version = "15.0")
```

## Arguments

- version:

  Version of MS SQL to simulate. Currently, 11.0 and above will use
  `TRY_CAST()` instead of `CAST()`, and 17.0 and above will support
  regular expression patterns in stringr functions.

## Bit vs boolean

SQL server uses two incompatible types to represent `TRUE` and `FALSE`
values:

- The `BOOLEAN` type is the result of logical comparisons (e.g. `x > y`)
  and can be used `WHERE` but not to create new columns in `SELECT`.
  <https://learn.microsoft.com/en-us/sql/t-sql/language-elements/comparison-operators-transact-sql>

- The `BIT` type is a special type of numeric column used to store
  `TRUE` and `FALSE` values, but can't be used in `WHERE` clauses.
  <https://learn.microsoft.com/en-us/sql/t-sql/data-types/bit-transact-sql?view=sql-server-ver15>

dbplyr does its best to automatically create the correct type when
needed, but can't do it 100% correctly because it does not have a full
type inference system. This means that you many need to manually do
conversions from time to time.

- To convert from bit to boolean use `x == 1`

- To convert from boolean to bit use `as.logical(if(x, 0, 1))`

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_mssql())
lf |> head()
#> <SQL>
#> SELECT TOP 6 [df].*
#> FROM [df]
lf |> transmute(x = paste(b, c, d))
#> <SQL>
#> SELECT [b] + ' ' + [c] + ' ' + [d] AS [x]
#> FROM [df]

# Can use boolean as is:
lf |> filter(c > d)
#> <SQL>
#> SELECT [df].*
#> FROM [df]
#> WHERE ([c] > [d])
# Need to convert from boolean to bit:
lf |> transmute(x = c > d)
#> <SQL>
#> SELECT CAST(IIF([c] > [d], 1, 0) AS BIT) AS [x]
#> FROM [df]
# Can use boolean as is:
lf |> transmute(x = ifelse(c > d, "c", "d"))
#> <SQL>
#> SELECT CASE WHEN ([c] > [d]) THEN 'c' WHEN NOT ([c] > [d]) THEN 'd' END AS [x]
#> FROM [df]
```
