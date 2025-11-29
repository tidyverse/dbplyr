# Copy a local data frame to a remote database

This is an implementation of the dplyr
[`dplyr::copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
generic and it mostly a wrapper around
[`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html).

It is useful for copying small amounts of data to a database for
examples, experiments, and joins. By default, it creates temporary
tables which are only visible within the current connection to the
database.

## Usage

``` r
# S3 method for class 'src_sql'
copy_to(
  dest,
  df,
  name = deparse(substitute(df)),
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  ...,
  in_transaction = TRUE
)
```

## Arguments

- dest:

  remote data source

- df:

  A local data frame, a `tbl_sql` from same source, or a `tbl_sql` from
  another source. If from another source, all data must transition
  through R in one pass, so it is only suitable for transferring small
  amounts of data.

- name:

  Name of new remote table. Use a string to create the table in the
  current catalog/schema. Use [`I()`](https://rdrr.io/r/base/AsIs.html)
  if you want to create it in a specific catalog/schema, e.g.
  `I("schema.table")`.

- overwrite:

  If `TRUE`, will overwrite an existing table with name `name`. If
  `FALSE`, will throw an error if `name` already exists.

- types:

  a character vector giving variable types to use for the columns. See
  <https://www.sqlite.org/datatype3.html> for available types.

- temporary:

  if `TRUE`, will create a temporary table that is local to this
  connection and will be automatically deleted when the connection
  expires

- unique_indexes:

  a list of character vectors. Each element of the list will create a
  new unique index over the specified column(s). Duplicate rows will
  result in failure.

- indexes:

  a list of character vectors. Each element of the list will create a
  new index.

- analyze:

  if `TRUE` (the default), will automatically ANALYZE the new table so
  that the query optimiser has useful information.

- ...:

  other parameters passed to methods.

- in_transaction:

  Should the table creation be wrapped in a transaction? This typically
  makes things faster, but you may want to suppress if the database
  doesn't support transactions, or you're wrapping in a transaction
  higher up (and your database doesn't support nested transactions.)

## Value

Another `tbl_lazy`. Use
[`show_query()`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
to see the generated query, and use
[`collect()`](https://dbplyr.tidyverse.org/dev/reference/collapse.tbl_sql.md)
to execute the query and return data to R.

## See also

[`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
to use small data in an SQL query without actually writing to a table.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

df <- data.frame(x = 1:5, y = letters[5:1])
db <- copy_to(src_memdb(), df)
db
#> # Source:   table<`df`> [?? x 2]
#> # Database: sqlite 3.51.0 [:memory:]
#>       x y    
#>   <int> <chr>
#> 1     1 e    
#> 2     2 d    
#> 3     3 c    
#> 4     4 b    
#> 5     5 a    

df2 <- data.frame(y = c("a", "d"), fruit = c("apple", "date"))
# copy_to() is called automatically if you set copy = TRUE
# in the join functions
db |> left_join(df2, copy = TRUE)
#> Joining with `by = join_by(y)`
#> # Source:   SQL [?? x 3]
#> # Database: sqlite 3.51.0 [:memory:]
#>       x y     fruit
#>   <int> <chr> <chr>
#> 1     1 e     NA   
#> 2     2 d     date 
#> 3     3 c     NA   
#> 4     4 b     NA   
#> 5     5 a     apple
```
