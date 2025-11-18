# Extract and check the `RETURNING` rows

**\[experimental\]**

`get_returned_rows()` extracts the `RETURNING` rows produced by
[`dplyr::rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_append()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html),
or
[`dplyr::rows_delete()`](https://dplyr.tidyverse.org/reference/rows.html)
if these are called with the `returning` argument. An error is raised if
this information is not available.

`has_returned_rows()` checks if `x` has stored RETURNING rows produced
by
[`dplyr::rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_append()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
[`dplyr::rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html),
or
[`dplyr::rows_delete()`](https://dplyr.tidyverse.org/reference/rows.html).

## Usage

``` r
get_returned_rows(x)

has_returned_rows(x)
```

## Arguments

- x:

  A lazy tbl.

## Value

For `get_returned_rows()`, a tibble.

For `has_returned_rows()`, a scalar logical.

## Examples

``` r
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbExecute(con, "CREATE TABLE Info (
   id INTEGER PRIMARY KEY AUTOINCREMENT,
   number INTEGER
)")
#> [1] 0
info <- tbl(con, "Info")

rows1 <- copy_inline(con, data.frame(number = c(1, 5)))
rows_insert(info, rows1, conflict = "ignore", in_place = TRUE)
#> Matching, by = "number"
info
#> # Source:   table<`Info`> [?? x 2]
#> # Database: sqlite 3.51.0 [:memory:]
#>      id number
#>   <int>  <int>
#> 1     1      1
#> 2     2      5

# If the table has an auto incrementing primary key, you can use
# the returning argument + `get_returned_rows()` its value
rows2 <- copy_inline(con, data.frame(number = c(13, 27)))
info <- rows_insert(
  info,
  rows2,
  conflict = "ignore",
  in_place = TRUE,
  returning = id
)
#> Matching, by = "number"
info
#> # Source:   table<`Info`> [?? x 2]
#> # Database: sqlite 3.51.0 [:memory:]
#>      id number
#>   <int>  <int>
#> 1     1      1
#> 2     2      5
#> 3     3     13
#> 4     4     27
get_returned_rows(info)
#> # A tibble: 2 × 1
#>      id
#>   <int>
#> 1     3
#> 2     4
```
