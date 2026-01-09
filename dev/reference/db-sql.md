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
  database will use. It can return character vector, in which case the
  first n elements are used to generate the plan and the final element
  is used to return the query plan.

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

sql_returning_cols(con, cols, table, ...)

sql_query_multi_join(
  con,
  x,
  joins,
  table_names,
  by_list,
  select,
  where = NULL,
  ...,
  distinct = FALSE,
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

sql_query_semi_join(con, x, y, anti, by, where, vars, ..., lvl = 0)

sql_set_op_method(con, op, ...)

sql_query_set_op(con, x, y, method, ..., lvl = 0)

sql_query_union(con, x, unions, ..., lvl = 0)
```

## See also

Other generic:
[`db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md),
[`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md),
[`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
