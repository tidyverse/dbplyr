# Package index

## dplyr verbs

### Connecting, copying, and retrieving data

- [`tbl(`*`<src_dbi>`*`)`](https://dbplyr.tidyverse.org/dev/reference/tbl.src_dbi.md)
  : Create a lazy query backed by a database
- [`collapse(`*`<tbl_sql>`*`)`](https://dbplyr.tidyverse.org/dev/reference/collapse.tbl_sql.md)
  : Collapse a query into a subquery
- [`collect(`*`<tbl_sql>`*`)`](https://dbplyr.tidyverse.org/dev/reference/collect.tbl_sql.md)
  : Collect results into a local data frame
- [`compute(`*`<tbl_sql>`*`)`](https://dbplyr.tidyverse.org/dev/reference/compute.tbl_sql.md)
  : Save results into a new remote table
- [`copy_to(`*`<src_sql>`*`)`](https://dbplyr.tidyverse.org/dev/reference/copy_to.src_sql.md)
  : Copy a local data frame to a remote database
- [`copy_inline()`](https://dbplyr.tidyverse.org/dev/reference/copy_inline.md)
  : Use a local data frame in a dbplyr query
- [`pull(`*`<tbl_sql>`*`)`](https://dbplyr.tidyverse.org/dev/reference/pull.tbl_sql.md)
  : Extract a single column
- [`show_query(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  [`explain(`*`<tbl_sql>`*`)`](https://dbplyr.tidyverse.org/dev/reference/show_query.md)
  : Show generated SQL and query plan
- [`last_sql()`](https://dbplyr.tidyverse.org/dev/reference/last_sql.md)
  : Retrieve the last SQL query generated
- [`.sql`](https://dbplyr.tidyverse.org/dev/reference/dot-sql.md) : Flag
  SQL function usage

### Verbs that affect rows

- [`arrange(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/arrange.tbl_lazy.md)
  : Arrange rows by column values
- [`distinct(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/distinct.tbl_lazy.md)
  : Subset distinct/unique rows
- [`filter(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/filter.tbl_lazy.md)
  : Subset rows using column values
- [`head(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/head.tbl_lazy.md)
  : Subset the first rows
- [`slice_min(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/dbplyr-slice.md)
  [`slice_max(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/dbplyr-slice.md)
  [`slice_sample(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/dbplyr-slice.md)
  : Subset rows using their positions

### Verbs that affect columns

- [`mutate(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/mutate.tbl_lazy.md)
  : Create, modify, and delete columns
- [`select(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/select.tbl_lazy.md)
  [`rename(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/select.tbl_lazy.md)
  [`rename_with(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/select.tbl_lazy.md)
  [`relocate(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/select.tbl_lazy.md)
  : Subset, rename, and reorder columns using their names

### Grouping and summarising verbs

- [`count(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/count.tbl_lazy.md)
  [`add_count(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/count.tbl_lazy.md)
  [`tally(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/count.tbl_lazy.md)
  : Count observations by group
- [`group_by(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/group_by.tbl_lazy.md)
  : Group by one or more variables
- [`summarise(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/summarise.tbl_lazy.md)
  : Summarise each group to one row
- [`do(`*`<tbl_sql>`*`)`](https://dbplyr.tidyverse.org/dev/reference/do.tbl_sql.md)
  **\[deprecated\]** : Perform arbitrary computation on remote backend

### Verbs that work with multiple tables

- [`bind_queries()`](https://dbplyr.tidyverse.org/dev/reference/bind_queries.md)
  : Combine multiple lazy queries
- [`inner_join(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/join.tbl_sql.md)
  [`left_join(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/join.tbl_sql.md)
  [`right_join(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/join.tbl_sql.md)
  [`full_join(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/join.tbl_sql.md)
  [`cross_join(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/join.tbl_sql.md)
  [`semi_join(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/join.tbl_sql.md)
  [`anti_join(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/join.tbl_sql.md)
  : Join SQL tables
- [`intersect(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/intersect.tbl_lazy.md)
  [`union(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/intersect.tbl_lazy.md)
  [`union_all(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/intersect.tbl_lazy.md)
  [`setdiff(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/intersect.tbl_lazy.md)
  : SQL set operations

### Verbs that modify the underlying data

- [`rows_insert(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/rows-db.md)
  [`rows_append(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/rows-db.md)
  [`rows_update(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/rows-db.md)
  [`rows_patch(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/rows-db.md)
  [`rows_upsert(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/rows-db.md)
  [`rows_delete(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/rows-db.md)
  : Edit individual rows in the underlying database table

- [`get_returned_rows()`](https://dbplyr.tidyverse.org/dev/reference/get_returned_rows.md)
  [`has_returned_rows()`](https://dbplyr.tidyverse.org/dev/reference/get_returned_rows.md)
  **\[experimental\]** :

  Extract and check the `RETURNING` rows

## tidyr verbs

- [`complete(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/complete.tbl_lazy.md)
  : Complete a SQL table with missing combinations of data
- [`dbplyr_uncount()`](https://dbplyr.tidyverse.org/dev/reference/dbplyr_uncount.md)
  : "Uncount" a database table
- [`expand(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/expand.tbl_lazy.md)
  : Expand SQL tables to include all possible combinations of values
- [`fill(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/fill.tbl_lazy.md)
  : Fill in missing values with previous or next value
- [`pivot_longer(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/pivot_longer.tbl_lazy.md)
  : Pivot data from wide to long
- [`pivot_wider(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/pivot_wider.tbl_lazy.md)
  [`dbplyr_pivot_wider_spec()`](https://dbplyr.tidyverse.org/dev/reference/pivot_wider.tbl_lazy.md)
  : Pivot data from long to wide
- [`replace_na(`*`<tbl_lazy>`*`)`](https://dbplyr.tidyverse.org/dev/reference/replace_na.tbl_lazy.md)
  : Replace NAs with specified values

## Built-in database backends

- [`simulate_access()`](https://dbplyr.tidyverse.org/dev/reference/backend-access.md)
  : Backend: MS Access
- [`simulate_hana()`](https://dbplyr.tidyverse.org/dev/reference/backend-hana.md)
  : Backend: SAP HANA
- [`simulate_hive()`](https://dbplyr.tidyverse.org/dev/reference/backend-hive.md)
  : Backend: Hive
- [`simulate_impala()`](https://dbplyr.tidyverse.org/dev/reference/backend-impala.md)
  : Backend: Impala
- [`simulate_mssql()`](https://dbplyr.tidyverse.org/dev/reference/backend-mssql.md)
  : Backend: SQL server
- [`simulate_mysql()`](https://dbplyr.tidyverse.org/dev/reference/backend-mysql.md)
  [`simulate_mariadb()`](https://dbplyr.tidyverse.org/dev/reference/backend-mysql.md)
  : Backend: MySQL/MariaDB
- [`simulate_odbc()`](https://dbplyr.tidyverse.org/dev/reference/backend-odbc.md)
  : Backend: ODBC
- [`simulate_oracle()`](https://dbplyr.tidyverse.org/dev/reference/backend-oracle.md)
  : Backend: Oracle
- [`simulate_postgres()`](https://dbplyr.tidyverse.org/dev/reference/backend-postgres.md)
  : Backend: PostgreSQL
- [`simulate_redshift()`](https://dbplyr.tidyverse.org/dev/reference/backend-redshift.md)
  : Backend: Redshift
- [`simulate_snowflake()`](https://dbplyr.tidyverse.org/dev/reference/backend-snowflake.md)
  : Backend: Snowflake
- [`simulate_spark_sql()`](https://dbplyr.tidyverse.org/dev/reference/backend-spark-sql.md)
  : Backend: Databricks Spark SQL
- [`simulate_sqlite()`](https://dbplyr.tidyverse.org/dev/reference/backend-sqlite.md)
  : Backend: SQLite
- [`simulate_teradata()`](https://dbplyr.tidyverse.org/dev/reference/backend-teradata.md)
  : Backend: Teradata

## Database connection

- [`memdb()`](https://dbplyr.tidyverse.org/dev/reference/memdb.md)
  [`memdb_frame()`](https://dbplyr.tidyverse.org/dev/reference/memdb.md)
  [`local_memdb_frame()`](https://dbplyr.tidyverse.org/dev/reference/memdb.md)
  : A temporary in-memory database
- [`remote_name()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  [`remote_table()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  [`remote_src()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  [`remote_con()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  [`remote_query()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  [`remote_query_plan()`](https://dbplyr.tidyverse.org/dev/reference/remote_name.md)
  : Metadata about a remote table

## SQL generation

- [`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_ident()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_logical()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_date()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_datetime()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_string()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_raw()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_vector()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  : Escape/quote a value
- [`sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md)
  [`is.sql()`](https://dbplyr.tidyverse.org/dev/reference/sql.md) :
  Literal SQL escaping
- [`sql_glue()`](https://dbplyr.tidyverse.org/dev/reference/sql_glue.md)
  [`sql_glue2()`](https://dbplyr.tidyverse.org/dev/reference/sql_glue.md)
  : Build SQL strings with glue syntax
- [`translate_sql()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
  [`translate_sql_()`](https://dbplyr.tidyverse.org/dev/reference/translate_sql.md)
  : Translate an expression to SQL
- [`window_order()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  [`window_frame()`](https://dbplyr.tidyverse.org/dev/reference/window_order.md)
  : Override window order and frame
- [`sql_options()`](https://dbplyr.tidyverse.org/dev/reference/sql_options.md)
  : Options for generating SQL

## dbplyr Backends

Documentation for authors of new dbplyr backends

- [`db_copy_to()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  [`db_compute()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  [`db_collect()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  [`sql_table_temporary()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  [`db_table_drop_if_exists()`](https://dbplyr.tidyverse.org/dev/reference/db-io.md)
  : Database I/O generics
- [`db_connection_describe()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  [`sql_join_suffix()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  [`db_sql_render()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  [`db_col_types()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  [`dbplyr_edition()`](https://dbplyr.tidyverse.org/dev/reference/db-misc.md)
  : Miscellaneous database generics
- [`sql_expr_matches()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_translation()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_random()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_table_analyze()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_table_index()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_explain()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_fields()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_save()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_wrap()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_indent_subquery()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_rows()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_returning_cols()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_multi_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_select()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_semi_join()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_set_op_method()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_set_op()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  [`sql_query_union()`](https://dbplyr.tidyverse.org/dev/reference/db-sql.md)
  : SQL generation generics
- [`escape()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_ident()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_logical()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_date()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_datetime()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_string()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_escape_raw()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  [`sql_vector()`](https://dbplyr.tidyverse.org/dev/reference/escape.md)
  : Escape/quote a value
- [`sql_dialect()`](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
  [`new_sql_dialect()`](https://dbplyr.tidyverse.org/dev/reference/sql_dialect.md)
  : SQL dialects
- [`sql_glue()`](https://dbplyr.tidyverse.org/dev/reference/sql_glue.md)
  [`sql_glue2()`](https://dbplyr.tidyverse.org/dev/reference/sql_glue.md)
  : Build SQL strings with glue syntax
- [`sql_variant()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`sql_translator()`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`base_scalar`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`base_agg`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`base_win`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`base_no_win`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`base_odbc_scalar`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`base_odbc_agg`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  [`base_odbc_win`](https://dbplyr.tidyverse.org/dev/reference/sql_variant.md)
  : Create an SQL translator
- [`sql_query_insert()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  [`sql_query_append()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  [`sql_query_update_from()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  [`sql_query_upsert()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  [`sql_query_delete()`](https://dbplyr.tidyverse.org/dev/reference/sql_query_insert.md)
  : Generate SQL for Insert, Update, Upsert, and Delete
- [`sql_infix()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  [`sql_prefix()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  [`sql_cast()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  [`sql_try_cast()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  [`sql_log()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  [`sql_cot()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  [`sql_runif()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_scalar.md)
  : SQL helpers for scalar functions
- [`sql_substr()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md)
  [`sql_str_sub()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md)
  [`sql_paste()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md)
  [`sql_paste_infix()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_string.md)
  : SQL helpers for string functions
- [`sql_aggregate()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  [`sql_aggregate_2()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  [`sql_aggregate_n()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  [`sql_check_na_rm()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  [`sql_not_supported()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_agg.md)
  : SQL helpers for aggregate functions
- [`win_over()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_rank()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_aggregate()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_aggregate_2()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_cumulative()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_absent()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_current_group()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_current_order()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  [`win_current_frame()`](https://dbplyr.tidyverse.org/dev/reference/sql_translation_window.md)
  : SQL helpers for window functions
