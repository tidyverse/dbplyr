# Join SQL tables

These are methods for the dplyr
[dplyr::join](https://dplyr.tidyverse.org/reference/mutate-joins.html)
generics. They are translated to the following SQL queries:

- `inner_join(x, y)`: `SELECT * FROM x JOIN y ON x.a = y.a`

- `left_join(x, y)`: `SELECT * FROM x LEFT JOIN y ON x.a = y.a`

- `right_join(x, y)`: `SELECT * FROM x RIGHT JOIN y ON x.a = y.a`

- `full_join(x, y)`: `SELECT * FROM x FULL JOIN y ON x.a = y.a`

- `semi_join(x, y)`:
  `SELECT * FROM x WHERE EXISTS (SELECT 1 FROM y WHERE x.a = y.a)`

- `anti_join(x, y)`:
  `SELECT * FROM x WHERE NOT EXISTS (SELECT 1 FROM y WHERE x.a = y.a)`

## Usage

``` r
# S3 method for class 'tbl_lazy'
inner_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = NULL,
  ...,
  keep = NULL,
  na_matches = c("never", "na"),
  multiple = NULL,
  unmatched = "drop",
  relationship = NULL,
  sql_on = NULL,
  auto_index = FALSE,
  x_as = NULL,
  y_as = NULL
)

# S3 method for class 'tbl_lazy'
left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = NULL,
  ...,
  keep = NULL,
  na_matches = c("never", "na"),
  multiple = NULL,
  unmatched = "drop",
  relationship = NULL,
  sql_on = NULL,
  auto_index = FALSE,
  x_as = NULL,
  y_as = NULL
)

# S3 method for class 'tbl_lazy'
right_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = NULL,
  ...,
  keep = NULL,
  na_matches = c("never", "na"),
  multiple = NULL,
  unmatched = "drop",
  relationship = NULL,
  sql_on = NULL,
  auto_index = FALSE,
  x_as = NULL,
  y_as = NULL
)

# S3 method for class 'tbl_lazy'
full_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = NULL,
  ...,
  keep = NULL,
  na_matches = c("never", "na"),
  multiple = NULL,
  relationship = NULL,
  sql_on = NULL,
  auto_index = FALSE,
  x_as = NULL,
  y_as = NULL
)

# S3 method for class 'tbl_lazy'
cross_join(
  x,
  y,
  ...,
  copy = FALSE,
  suffix = c(".x", ".y"),
  x_as = NULL,
  y_as = NULL
)

# S3 method for class 'tbl_lazy'
semi_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  ...,
  na_matches = c("never", "na"),
  sql_on = NULL,
  auto_index = FALSE,
  x_as = NULL,
  y_as = NULL
)

# S3 method for class 'tbl_lazy'
anti_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  ...,
  na_matches = c("never", "na"),
  sql_on = NULL,
  auto_index = FALSE,
  x_as = NULL,
  y_as = NULL
)
```

## Arguments

- x, y:

  A pair of lazy data frames backed by database queries.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into a temporary table in same
  database as `x`. `*_join()` will automatically run `ANALYZE` on the
  created table in the hope that this will make you queries as efficient
  as possible by giving more data to the query planner.

  This allows you to join tables across srcs, but it's potentially
  expensive operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- ...:

  Other parameters passed onto methods.

- keep:

  Should the join keys from both `x` and `y` be preserved in the output?

  - If `NULL`, the default, joins on equality retain only the keys from
    `x`, while joins on inequality retain the keys from both inputs.

  - If `TRUE`, all keys from both inputs are retained.

  - If `FALSE`, only keys from `x` are retained. For right and full
    joins, the data in key columns corresponding to rows that only exist
    in `y` are merged into the key columns from `x`. Can't be used when
    joining on inequality conditions.

- na_matches:

  Should NA (NULL) values match one another? The default, "never", is
  how databases usually work. `"na"` makes the joins behave like the
  dplyr join functions, [`merge()`](https://rdrr.io/r/base/merge.html),
  [`bit64::match()`](https://rdrr.io/pkg/bit64/man/bit64S3.html), and
  `%in%`.

- multiple, unmatched:

  Unsupported in database backends. As a workaround for multiple use a
  unique key and for unmatched a foreign key constraint.

- relationship:

  Unsupported in database backends.

- sql_on:

  A custom join predicate as an SQL expression. Usually joins use column
  equality, but you can perform more complex queries by supply `sql_on`
  which should be a SQL expression that uses `LHS` and `RHS` aliases to
  refer to the left-hand side or right-hand side of the join
  respectively.

- auto_index:

  if `copy` is `TRUE`, automatically create indices for the variables in
  `by`. This may speed up the join if there are matching indexes in `x`.

- x_as, y_as:

  Alias to use for `x` resp. `y`. Defaults to `"LHS"` resp. `"RHS"`

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collapse.tbl_sql.md)
to execute the query and return data to R.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

band_db <- tbl_memdb(dplyr::band_members)
instrument_db <- tbl_memdb(dplyr::band_instruments)
band_db %>% left_join(instrument_db) %>% show_query()
#> Joining with `by = join_by(name)`
#> <SQL>
#> SELECT `dplyr::band_members`.*, `plays`
#> FROM `dplyr::band_members`
#> LEFT JOIN `dplyr::band_instruments`
#>   ON (`dplyr::band_members`.`name` = `dplyr::band_instruments`.`name`)

# Can join with local data frames by setting copy = TRUE
band_db %>%
  left_join(dplyr::band_instruments, copy = TRUE)
#> Joining with `by = join_by(name)`
#> # Source:   SQL [?? x 3]
#> # Database: sqlite 3.51.0 [:memory:]
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 Mick  Stones  NA    
#> 2 John  Beatles guitar
#> 3 Paul  Beatles bass  

# Unlike R, joins in SQL don't usually match NAs (NULLs)
db <- memdb_frame(x = c(1, 2, NA))
label <- memdb_frame(x = c(1, NA), label = c("one", "missing"))
db %>% left_join(label, by = "x")
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.0 [:memory:]
#>       x label
#>   <dbl> <chr>
#> 1     1 one  
#> 2     2 NA   
#> 3    NA NA   
# But you can activate R's usual behaviour with the na_matches argument
db %>% left_join(label, by = "x", na_matches = "na")
#> # Source:   SQL [?? x 2]
#> # Database: sqlite 3.51.0 [:memory:]
#>       x label  
#>   <dbl> <chr>  
#> 1     1 one    
#> 2     2 NA     
#> 3    NA missing

# By default, joins are equijoins, but you can use `sql_on` to
# express richer relationships
db1 <- memdb_frame(x = 1:5)
db2 <- memdb_frame(x = 1:3, y = letters[1:3])
db1 %>% left_join(db2) %>% show_query()
#> Joining with `by = join_by(x)`
#> <SQL>
#> SELECT `dbplyr_GrnDDOhsrA`.`x` AS `x`, `y`
#> FROM `dbplyr_GrnDDOhsrA`
#> LEFT JOIN `dbplyr_7gFZ2RIqws`
#>   ON (`dbplyr_GrnDDOhsrA`.`x` = `dbplyr_7gFZ2RIqws`.`x`)
db1 %>% left_join(db2, sql_on = "LHS.x < RHS.x") %>% show_query()
#> <SQL>
#> SELECT `LHS`.`x` AS `x.x`, `RHS`.`x` AS `x.y`, `y`
#> FROM `dbplyr_GrnDDOhsrA` AS `LHS`
#> LEFT JOIN `dbplyr_7gFZ2RIqws` AS `RHS`
#>   ON (LHS.x < RHS.x)
```
