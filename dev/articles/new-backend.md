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
#> # Database: sqlite 3.51.1 []
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> 5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
#> 6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1
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

## SQL translation: verbs

Make sure you’ve read
[`vignette("translation-verb")`](https://dbplyr.tidyverse.org/dev/articles/translation-verb.md)
so you have the lay of the land. First check that SQL translation for
the key verbs work:

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

## SQL translation: vectors

Finally, you may have to provide custom R -\> SQL translation for
functions that work with vectors within verbs. You can do with by
providing a method for
[`sql_translation()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
which return an object created by
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md).
The
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
function creates a container for three types of function translations:

``` r
sql_translation.myConnectionClass <- function(con) {
  sql_variant(
    scalar = sql_translator(base_scalar, ...), # Functions in SELECT (non-aggregated)
    aggregate = sql_translator(base_aggregate, ...), # Aggregation functions (mean, sum, etc.)
    window = sql_translator(base_win, ...) # Window functions (lead, lag, rank, etc.)
  )
}
```

Each translator will inherits from the base (ANSI SQL) translator and
overrides only what’s different for your backend:

``` r
sql_translator(
  base_scalar, # Inherit most translations
  # Override specific functions for your backend
  `+` = sql_infix("+"),
  mean = sql_aggregate("AVERAGE", "mean")
)
```

### Scalar function helpers

dbplyr provides several helper functions to make it easier to translate
R functions to SQL:

- **`sql_prefix(f, n = NULL)`**: For standard SQL functions. The `n`
  argument optionally specifies the number of arguments.
- **`sql_infix(f)`**: For infix operators like `+`, `*`, or `==`.
- **`sql_cast(type)`**: For type casting functions.
- **`sql_not_supported(f)`**: For functions with no SQL translation.

Here’s an example showing all of these helpers in use:

``` r
sql_translation.myConnectionClass <- function(con) {
  sql_variant(
    scalar = sql_translator(
      base_scalar,
      # Standard SQL functions
      cos = sql_prefix("COS", 1),
      round = sql_prefix("ROUND", 2),
      # Infix operators
      `+` = sql_infix("+"),
      `*` = sql_infix("*"),
      `==` = sql_infix("="),
      # Type casting
      as.numeric = sql_cast("NUMERIC"),
      as.character = sql_cast("VARCHAR")
    ),
    aggregate = sql_translator(base_agg),
    window = sql_translator(base_win)
  )
}
```

### Aggregation function helpers

- **`sql_aggregate(f, f_r = f)`**: For single-argument SQL aggregate
  functions. The `f_r` argument gives the name of the R function.
- **`sql_aggregate_2(f)`**: For two-argument SQL aggregate functions.
- **`sql_aggregate_n(f, f_r = f)`**: For variadic SQL aggregate
  functions.

``` r
sql_translation.myConnectionClass <- function(con) {
  sql_variant(
    scalar = sql_translator(base_scalar),
    aggregate = sql_translator(
      .parent = base_agg,
      # Single-argument aggregates
      mean = sql_aggregate("AVG", "mean"),
      var = sql_aggregate("VAR_SAMP", "var"),
      # Two-argument aggregates
      cov = sql_aggregate_2("COVAR_SAMP"),
      # Variadic aggregates
      pmin = sql_aggregate_n("LEAST", "pmin"),
      pmax = sql_aggregate_n("GREATEST", "pmax"),
      # Unsupported functions
      median = sql_not_supported("median")
    ),
    window = sql_translator(base_win)
  )
}
```

### Window function helpers

Window functions have their own set of helpers:

- **`win_rank(f)`**: For ranking functions.
- **`win_aggregate(f)`**: For aggregate functions used as window
  functions.
- **`win_cumulative(f)`**: For cumulative functions.
- **`win_absent(f)`**: For backends that don’t support certain window
  functions.

Here’s an example showing all of these helpers in use:

``` r
window = sql_translator(
  base_win,
  # Ranking functions
  row_number = win_rank("ROW_NUMBER"),
  rank = win_rank("RANK"),
  dense_rank = win_rank("DENSE_RANK"),
  # Aggregate functions as window functions
  mean = win_aggregate("AVG"),
  sum = win_aggregate("SUM"),
  # Cumulative functions
  cumsum = win_cumulative("SUM"),
  # Absent functions
  cume_dist = win_absent("cume_dist")
)
```

### Custom translation functions

For more complex translations, you can write custom functions that
return SQL expressions using
[`sql_expr()`](https://dbplyr.tidyverse.org/dev/reference/sql_expr.md).
This is a tidy-evaluation function that allows you to use `!!` operator
to in inject a single value into SQL expressions or `!!!` to splice a
list of values.

``` r
scalar = sql_translator(
  base_scalar,

  # Custom log function with change of base
  log = function(x, base = exp(1)) {
    if (isTRUE(all.equal(base, exp(1)))) {
      sql_expr(ln(!!x))
    } else {
      sql_expr(log(!!x) / log(!!base))
    }
  },

  # Custom paste function using CONCAT
  paste = function(..., sep = " ") {
    args <- list(...)
    sql_expr(CONCAT_WS(!!sep, !!!args))
  }
)
```
