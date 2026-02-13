# Developing a dbplyr backend

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
library(dbplyr)
library(DBI)
```

Check that you can create a tbl from a connection, like:

``` r
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
DBI::dbWriteTable(con, "mtcars", mtcars)

tbl(con, "mtcars")
#> # A query:  ?? x 11
#> # Database: sqlite 3.51.2 []
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

## Define a dialect

Next, define a dialect for your backend using
[`sql_dialect()`](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
and
[`new_sql_dialect()`](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md).
A dialect encapsulates the SQL syntax rules for your database:

``` r
#' @export
sql_dialect.myConnectionClass <- function(con) {
  new_sql_dialect(
    "mybackend",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}
```

The
[`new_sql_dialect()`](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
function requires two arguments:

- `dialect`: A unique name for your backend (used to create the class
  name).
- `quote_identifier`: A function that quotes identifiers. This will
  typically b e either a call to
  [`sql_quote()`](https://dbplyr.tidyverse.org/dev/reference/sql_quote.md)
  or to
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)

[`new_sql_dialect()`](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
has a number of additional arguments that describe the capabilities of
the SQL dialect; see
[`?new_sql_dialect`](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
for details.

The dialect system allows you to write methods that dispatch on the
dialect class (e.g., `sql_translation.sql_dialect_mybackend`) rather
than the connection class. This makes it easier to share SQL generation
code between different connection types that use the same database
(e.g. direct, ODBC, JDBC, ADBC).

In general, generics that start with `sql_` should have a sql_dialect
method, and generics that start with `db_` should use a DBI connection
method. It should be relatively rare to need a method for `db_` generic,
but occassionally dbplyr’s SQL generation system is not quite flexible
enough. If you find this, it’s worth filing an issue so I can look into
it.

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

### Building SQL strings

If you need to generate your own SQL, we recommend using
[`sql_glue2()`](https://dbplyr.tidyverse.org/dev/reference/sql_glue.md).
It uses glue syntax with type markers for safe SQL generation:

``` r
con <- simulate_dbi()

# Create an index
index_name <- "index"
table <- I("schema.table")
columns <- c("column1", "column2")
sql_glue2(con, "CREATE INDEX {.id index_name} ON {.tbl table} {.id columns*}")
#> <SQL> CREATE INDEX "index" ON schema.table ("column1", "column2")

# Insert values safely
name <- "O'Brien"
sql_glue2(con, "INSERT INTO students (name) VALUES {name*}")
#> <SQL> INSERT INTO students (name) VALUES ('O''Brien')

# Build a query
table <- "my_table"
cols <- c("id", "name", "value")
sql_glue2(con, "SELECT {.id cols} FROM {.tbl table}")
#> <SQL> SELECT "id", "name", "value" FROM "my_table"
```

### Behind the scenes

The high-level translation from R code to SQL takes place in a few
steps, using two intermediate representations: a “lazy query” and a
“query”. A lazy query is closer to R and a query is closer to SQL. This
continuum of representation allows us to work with expressions when
trying to reason about the transformation pipeline, then later switch to
strings of SQL when we get closer to generating a query.

All dbplyr pipelines are built on top of the `tbl_lazy` data structure,
which is the only data structure that the user will ever see. It has two
fields:

- A database `con`nection which is used to specialise SQL translation.
- A `lazy_query` which represents the lazy computation described by the
  pipeline. It always starts off as a `lazy_base_query` which represents
  a remote table or SQL query.

Each verb takes a `tbl_lazy` as input and returns a modified `tbl_lazy`,
modifying the `lazy_query` with the new requirements. At the simplest
level, this involves wrapping the existing lazy query within a new lazy
query, such as `lazy_query_select()` or `lazy_query_multi_join()`. These
represent what later will become a subquery so that (e.g.)
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html), and
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
all use
[`lazy_select_query()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md),
whereas
[`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
and
[`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
use `lazy_query_multi_join()`.

These lazy queries are typically created by a helper function like
`add_select()`, `add_mutate()`, and `add_filter()`. These helpers have
two roles:

1.  They abstract away repeated code for substantially similar dplyr
    verbs. For example,
    [`select()`](https://dplyr.tidyverse.org/reference/select.html),
    [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
    [`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html)
    and
    [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
    all use `add_select()`.

2.  They are responsible for figuring out if we can re-use the existing
    query or we need to wrap in a subquery. Typically this work is done
    by a function called `can_inline_select()` which will use what we
    know about SQL and the verb to determine whether or not we have to
    introduce a subquery.

The lazy query gives us a representation of the data manipulation
pipeline with enough detail that we can reason about when we need a
subquery. Then when the user calls
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html), we
need to generate an SQL query. This happens in two steps: building and
rendering.

First,
[`sql_build()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md)
recurses over the lazy query, building up query objects like
[`select_query()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md)
and `multi_join_query()` that represent the different subtypes of
`SELECT` queries. Compared to the previous “lazy” query representation,
these are much closer to SQL than R. They can be much simpler because we
no longer need to reason about subqueries, but we still need some
structure to facilitate backend-specific translations.

Next,
[`sql_render()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md)
is called on the result of
[`sql_build()`](https://dbplyr.tidyverse.org/dev/reference/sql_build.md)
and dispatches to generics like
[`sql_query_select()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
[`sql_query_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
[`sql_query_semi_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
and
[`sql_query_set_op()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md).
These generics have methods for each backend that are responsible for
creating the needed SQL.

## SQL translation: vectors

Finally, you may have to provide custom R -\> SQL translation for
functions that work with vectors within verbs. You can do this by
providing a method for
[`sql_translation()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md),
which returns an object created by
[`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md).

You can define translation methods either on your connection class or on
your dialect class. Using the dialect class is recommended as it allows
sharing translations between different connection types:

``` r
# Method on dialect class (recommended)
sql_translation.sql_dialect_mybackend <- function(con) {
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
sql_translation.sql_dialect_mybackend <- function(con) {
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
sql_translation.sql_dialect_mybackend <- function(con) {
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
[`sql_glue()`](https://dbplyr.tidyverse.org/dev/reference/sql_glue.md).
This uses glue syntax for string interpolation with automatic escaping.

``` r
scalar = sql_translator(
  base_scalar,

  # Custom log function with change of base
  log = function(x, base = exp(1)) {
    if (isTRUE(all.equal(base, exp(1)))) {
      sql_glue("LN({x})")
    } else {
      sql_glue("LOG({x}) / LOG({base})")
    }
  },

  # Custom paste function using CONCAT
  paste = function(..., sep = " ") {
    sql_glue("CONCAT_WS({sep}, {...})")
  }
)
```
