# SQL generation generics

SQL translation:

- `sql_expr_matches(con, x, y)` generates an alternative to `x = y` when
  a pair of `NULL`s should match. The default translation uses a
  `CASE WHEN` as described in
  <https://modern-sql.com/feature/is-distinct-from>.

- `sql_translation(con)` generates a SQL translation environment.

- Deprecated: `sql_random(con)` generates SQL to get a random number
  which can be used to select random rows in
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html).
  This is now replaced by adding a translation for `runif(n())`.

- `supports_window_clause(con)` does the backend support named windows?

- `db_supports_table_alias_with_as(con)` does the backend support using
  `AS` when using a table alias?

Tables:

- `sql_table_analyze(con, table)` generates SQL that "analyzes" the
  table, ensuring that the database has up-to-date statistics for use in
  the query planner. It called from
  [`dplyr::copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html)
  when `analyze = TRUE`.

- `sql_table_index()` generates SQL for adding an index to table.

Query manipulation:

- `sql_query_explain(con, sql)` generates SQL that "explains" a query,
  i.e. generates a query plan describing what indexes etc that the
  database will use.

- `sql_query_fields()` generates SQL for a 0-row result that is used to
  capture field names in
  [`tbl_sql()`](https://dbplyr.tidyverse.org/dev/reference/tbl_sql.md)

- `sql_query_save(con, sql)` generates SQL for saving a query into a
  (temporary) table.

- `sql_query_wrap(con, from)` generates SQL for wrapping a query into a
  subquery.

Query indentation:

- `sql_indent_subquery(from, con, lvl)` helps indenting a subquery.

Query generation:

- `sql_query_select()` generates SQL for a `SELECT` query

- `sql_query_join()` generates SQL for joins

- `sql_query_semi_join()` generates SQL for semi- and anti-joins

- `sql_query_set_op()` generates SQL for `UNION`, `INTERSECT`, and
  `EXCEPT` queries.

Query generation for manipulation:

- [`sql_query_insert()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  and
  [`sql_query_append()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  generate SQL for an `INSERT FROM` query.

- [`sql_query_update_from()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  generates SQL for an `UPDATE FROM` query.

- [`sql_query_upsert()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  generates SQL for an `UPSERT` query.

- [`sql_query_delete()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  generates SQL for an `DELETE FROM` query

- `sql_returning_cols()` generates SQL for a `RETURNING` clause

## Usage

``` r
sql_expr_matches(con, x, y, ...)

sql_translation(con)

sql_random(con)

sql_table_analyze(con, table, ...)

sql_table_index(
  con,
  table,
  columns,
  name = NULL,
  unique = FALSE,
  ...,
  call = caller_env()
)

sql_query_explain(con, sql, ...)

sql_query_fields(con, sql, ...)

sql_query_save(con, sql, name, temporary = TRUE, ...)

sql_query_wrap(con, from, name = NULL, ..., lvl = 0)

sql_indent_subquery(from, con, lvl = 0)

sql_query_rows(con, sql, ...)

supports_window_clause(con)

db_supports_table_alias_with_as(con)

sql_query_select(
  con,
  select,
  from,
  where = NULL,
  group_by = NULL,
  having = NULL,
  window = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  ...,
  subquery = FALSE,
  lvl = 0
)

sql_query_join(
  con,
  x,
  y,
  select,
  type = "inner",
  by = NULL,
  na_matches = FALSE,
  ...,
  lvl = 0
)

sql_query_multi_join(con, x, joins, table_names, by_list, select, ..., lvl = 0)

sql_query_semi_join(con, x, y, anti, by, where, vars, ..., lvl = 0)

sql_query_set_op(con, x, y, method, ..., all = FALSE, lvl = 0)

sql_query_union(con, x, unions, ..., lvl = 0)

sql_returning_cols(con, cols, table, ...)
```

## dbplyr 2.0.0

Many `dplyr::db_*` generics have been replaced by `dbplyr::sql_*`
generics. To update your backend, you'll need to extract the SQL
generation out of your existing code, and place it in a new method for a
dbplyr `sql_` generic.

- [`dplyr::db_analyze()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_table_analyze()`

- [`dplyr::db_explain()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_explain()`

- [`dplyr::db_create_index()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_table_index()`

- [`dplyr::db_query_fields()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_fields()`

- [`dplyr::db_query_rows()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is no longer used; you can delete it

- [`dplyr::db_save_query()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_save()`

The query generating functions have also changed names. Their behaviour
is unchanged, so you just need to rename the generic and import from
dbplyr instead of dplyr.

- [`dplyr::sql_select()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_select()`

- [`dplyr::sql_join()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_join()`

- [`dplyr::sql_semi_join()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_semi_join()`

- [`dplyr::sql_set_op()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_set_op()`

- [`dplyr::sql_subquery()`](https://dplyr.tidyverse.org/reference/backend_dbplyr.html)
  is replaced by `dbplyr::sql_query_wrap()`

Learn more in `vignette("backend-2.0")`

## See also

Other generic:
[`db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md),
[`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md),
[`sql_escape_logical()`](https://dbplyr.tidyverse.org/dev/reference/db-quote.md)
