# SQL escaping/quoting generics

These generics translate individual values into SQL. The core generics
are
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
and
[DBI::dbQuoteString](https://dbi.r-dbi.org/reference/dbQuoteString.html)
for quoting identifiers and strings, but dbplyr needs additional tools
for inserting logical, date, date-time, and raw values into queries.

## Usage

``` r
sql_escape_logical(con, x)

sql_escape_date(con, x)

sql_escape_datetime(con, x)

sql_escape_raw(con, x)
```

## See also

Other generic:
[`db-sql`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
[`db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md),
[`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)

## Examples

``` r
con <- simulate_dbi()
sql_escape_logical(con, c(TRUE, FALSE, NA))
#> [1] "TRUE"  "FALSE" "NULL" 
sql_escape_date(con, Sys.Date())
#> [1] "'2025-11-28'"
sql_escape_date(con, Sys.time())
#> [1] "'2025-11-28 08:31:14.789789'"
sql_escape_raw(con, charToRaw("hi"))
#> [1] "X'6869'"
```
