# Adding a new DBI backend

This document describes how to add a new SQL backend to dbplyr. To
begin:

- Ensure that you have a DBI compliant database backend. If not, you’ll
  need to first create it by following the instructions in
  [`vignette("backend", package = "DBI")`](https://dbi.r-dbi.org/articles/backend.html).

- You’ll need a working knowledge of S3. Make sure that you’re [familiar
  with the basics](https://adv-r.hadley.nz/s3.html) before you start.

This document is still a work in progress, but it will hopefully get you
started. I’d also strongly recommend reading the bundled source code for
[SQLite](https://github.com/tidyverse/dbplyr/blob/master/R/backend-sqlite.R),
[MySQL](https://github.com/tidyverse/dbplyr/blob/master/R/backend-mysql.R),
and
[PostgreSQL](https://github.com/tidyverse/dbplyr/blob/master/R/backend-postgres.R).

## First steps

For interactive exploitation, attach dplyr and DBI. If you’re creating a
package, you’ll need to import dplyr and DBI.

``` r
library(dplyr)
library(DBI)
```

Check that you can create a tbl from a connection, like:

``` r
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
DBI::dbWriteTable(con, "mtcars", mtcars)

tbl(con, "mtcars")
#> # Source:   table<`mtcars`> [?? x 11]
#> # Database: sqlite 3.51.0 []
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> # ℹ more rows
```

If you can’t, this likely indicates some problem with the DBI methods.
Use [DBItest](https://github.com/r-dbi/DBItest) to narrow down the
problem.

## Write your first method

The first method of your dbplyr backend should always be for the
[`dbplyr_edition()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
generic:

``` r
#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.myConnectionClass <- function(con) 2L
```

This declares that your package uses version 2 of the API, which is the
version that this vignette documents.

## Copying, computing, collecting and collapsing

Next, check that
[`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html),
[`collapse()`](https://dplyr.tidyverse.org/reference/compute.html),
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html), and
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) work:

- If [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
  fails, you probably need a method for
  [`sql_table_analyze()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  or
  [`sql_table_index()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md).
  If [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
  fails during creation of the tbl, you may need a method for
  [`sql_query_fields()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md).

- If [`collapse()`](https://dplyr.tidyverse.org/reference/compute.html)
  fails, your database has a non-standard way of constructing
  subqueries. Add a method for
  [`sql_subquery()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html).

- If [`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
  fails, your database has a non-standard way of saving queries in
  temporary tables. Add a method for
  [`db_save_query()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html).

## SQL translation

Make sure you’ve read
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
so you have the lay of the land.

### Verbs

Check that SQL translation for the key verbs work:

- [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) etc:
  powered by
  [`sql_query_select()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
- [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html):
  powered by
  [`sql_query_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
- [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
  [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html):
  powered by
  [`sql_query_semi_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
- [`union()`](https://generics.r-lib.org/reference/setops.html),
  [`intersect()`](https://generics.r-lib.org/reference/setops.html),
  [`setdiff()`](https://generics.r-lib.org/reference/setops.html):
  powered by
  [`sql_query_set_op()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)

### Vectors

Finally, you may have to provide custom R -\> SQL translation at the
vector level by providing a method for
[`sql_translate_env()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html).
This function should return an object created by
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md).
See existing methods for examples.
