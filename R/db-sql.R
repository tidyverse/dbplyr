#' SQL generation generics
#'
#' @description
#'
#' SQL translation:
#'
#' * `sql_expr_matches(con, x, y)` generates an alternative to `x = y` when a
#'   pair of `NULL`s should match. The default translation uses a `CASE WHEN`
#'   as described in <https://modern-sql.com/feature/is-distinct-from>.
#'
#' * `sql_translation(con)` generates a SQL translation environment.
#'
#' * Deprecated: `sql_random(con)` generates SQL to get a random number which can be used
#'   to select random rows in `slice_sample()`. This is now replaced by adding
#'   a translation for `runif(n())`.
#'
#' * `supports_window_clause(con)` does the backend support named windows?
#'
#' * `db_supports_table_alias_with_as(con)` does the backend support using `AS` when using a table alias?
#'
#' Tables:
#'
#' * `sql_table_analyze(con, table)` generates SQL that "analyzes" the table,
#'   ensuring that the database has up-to-date statistics for use in the query
#'   planner. It called from [copy_to()] when `analyze = TRUE`.
#'
#' * `sql_table_index()` generates SQL for adding an index to table.
#'
#' Query manipulation:
#'
#' * `sql_query_explain(con, sql)` generates SQL that "explains" a query,
#'   i.e. generates a query plan describing what indexes etc that the
#'   database will use. It can return character vector, in which case the
#'   first n elements are used to generate the plan and the final element
#'   is used to return the query plan.
#'
#' * `sql_query_fields()` generates SQL for a 0-row result that is used to
#'   capture field names in [tbl_sql()]
#'
#' * `sql_query_save(con, sql)` generates SQL for saving a query into a
#'   (temporary) table.
#'
#' * `sql_query_wrap(con, from)` generates SQL for wrapping a query into a
#'   subquery.
#'
#' Query indentation:
#'
#' * `sql_indent_subquery(from, con, lvl)` helps indenting a subquery.
#'
#' Query generation:
#'
#' * `sql_query_select()` generates SQL for a `SELECT` query
#' * `sql_query_join()` generates SQL for joins
#' * `sql_query_semi_join()` generates SQL for semi- and anti-joins
#' * `sql_query_set_op()` generates SQL for `UNION`, `INTERSECT`, and `EXCEPT`
#'   queries.
#'
#' Query generation for manipulation:
#'
#' * `sql_query_insert()` and `sql_query_append()` generate SQL for an `INSERT FROM` query.
#' * `sql_query_update_from()` generates SQL for an `UPDATE FROM` query.
#' * `sql_query_upsert()` generates SQL for an `UPSERT` query.
#' * `sql_query_delete()` generates SQL for an `DELETE FROM` query
#' * `sql_returning_cols()` generates SQL for a `RETURNING` clause
#'
#' @keywords internal
#' @family generic
#' @name db-sql
NULL

#' @export
#' @rdname db-sql
sql_expr_matches <- function(con, x, y, ...) {
  check_dots_used()
  UseMethod("sql_expr_matches")
}
# https://modern-sql.com/feature/is-distinct-from
#' @export
sql_expr_matches.DBIConnection <- function(con, x, y, ...) {
  glue_sql2(
    con,
    "CASE WHEN ({x} = {y}) OR ({x} IS NULL AND {y} IS NULL) ",
    "THEN 0 ",
    "ELSE 1 ",
    "END = 0"
  )
}

#' @export
#' @rdname db-sql
sql_translation <- function(con) {
  UseMethod("sql_translation")
}
# sql_translation.DBIConnection lives in backend-.R
dbplyr_sql_translation <- function(con) {
  check_2ed(con)
  sql_translation(con)
}

#' @export
#' @rdname db-sql
sql_random <- function(con) {
  lifecycle::deprecate_warn(
    "2.3.2",
    "sql_random()",
    with = I("Please add a translation for `runif(n())` instead.")
  )
  UseMethod("sql_random")
}


# Tables ------------------------------------------------------------------

#' @rdname db-sql
#' @export
sql_table_analyze <- function(con, table, ...) {
  check_dots_used()
  UseMethod("sql_table_analyze")
}
#' @export
sql_table_analyze.DBIConnection <- function(con, table, ...) {
  glue_sql2(con, "ANALYZE {.tbl {table}}")
}

#' @rdname db-sql
#' @export
sql_table_index <- function(
  con,
  table,
  columns,
  name = NULL,
  unique = FALSE,
  ...,
  call = caller_env()
) {
  check_table_id(table, call = call)
  check_character(columns, call = call)
  check_name(name, allow_null = TRUE, call = call)
  check_bool(unique, call = call)

  UseMethod("sql_table_index")
}
#' @export
sql_table_index.DBIConnection <- function(
  con,
  table,
  columns,
  name = NULL,
  unique = FALSE,
  ...,
  call = caller_env()
) {
  table <- as_table_path(table, con)

  if (is.null(name)) {
    table_name <- table_path_name(table, con)
    name <- name %||% paste0(c(table_name, columns), collapse = "_")
  }
  glue_sql2(
    con,
    "CREATE ",
    if (unique) "UNIQUE ",
    "INDEX {.name name}",
    " ON {.tbl table} ({.col columns*})"
  )
}

# Query manipulation ------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_explain <- function(con, sql, ...) {
  check_scalar_sql(sql)
  check_dots_used()
  UseMethod("sql_query_explain")
}
#' @export
sql_query_explain.DBIConnection <- function(con, sql, ...) {
  glue_sql2(con, "EXPLAIN {sql}")
}

#' @rdname db-sql
#' @export
sql_query_fields <- function(con, sql, ...) {
  check_table_source(sql)
  check_dots_used()

  UseMethod("sql_query_fields")
}
#' @export
sql_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- as_table_source(sql, con)
  dbplyr_query_select(
    con,
    sql("*"),
    dbplyr_sql_subquery(con, sql),
    where = sql("0 = 1")
  )
}

#' @rdname db-sql
#' @export
sql_query_save <- function(con, sql, name, temporary = TRUE, ...) {
  check_table_id(name)
  check_bool(temporary)
  check_dots_used()

  UseMethod("sql_query_save")
}
#' @export
sql_query_save.DBIConnection <- function(
  con,
  sql,
  name,
  temporary = TRUE,
  ...
) {
  name <- as_table_path(name, con)

  glue_sql2(
    con,
    "CREATE ",
    if (temporary) sql("TEMPORARY "),
    "TABLE \n",
    "{.tbl {name}} AS\n",
    "{.from sql}"
  )
}
#' @export
#' @rdname db-sql
sql_query_wrap <- function(con, from, name = NULL, ..., lvl = 0) {
  check_name(name, allow_null = TRUE)
  check_dots_used()

  UseMethod("sql_query_wrap")
}
#' @export
sql_query_wrap.DBIConnection <- function(con, from, name = NULL, ..., lvl = 0) {
  from <- as_table_source(from, con)

  if (is.sql(from)) {
    if (db_supports_table_alias_with_as(con)) {
      as_sql <- style_kw(" AS ")
    } else {
      as_sql <- " "
    }

    from <- sql_indent_subquery(from, con, lvl)
    # some backends, e.g. Postgres, require an alias for a subquery
    name <- name %||% unique_subquery_name()
    glue_sql2(con, "{from}", as_sql, "{.tbl name}")
  } else {
    # must be a table_path
    if (!is.null(name)) {
      table <- table_path_name(name, con)
      names(from) <- as_table_path(table, con)
    }
    from
  }
}

#' @export
#' @rdname db-sql
sql_indent_subquery <- function(from, con, lvl = 0) {
  multi_line <- grepl(x = from, pattern = "\\r\\n|\\r|\\n")
  if (multi_line) {
    out <- paste0(
      "(\n",
      from,
      "\n",
      indent_lvl(")", lvl)
    )
  } else {
    # Strip indent
    from <- gsub("^ +", "", from)
    out <- paste0("(", from, ")")
  }

  sql(out)
}

#' @rdname db-sql
#' @export
sql_query_rows <- function(con, sql, ...) {
  check_table_source(sql)
  check_dots_used()

  UseMethod("sql_query_rows")
}
#' @export
sql_query_rows.DBIConnection <- function(con, sql, ...) {
  sql <- as_table_source(sql, con)
  from <- dbplyr_sql_subquery(con, sql, "master")
  glue_sql2(con, "SELECT COUNT(*) FROM {.from from}")
}

#' @rdname db-sql
#' @export
supports_window_clause <- function(con) {
  UseMethod("supports_window_clause")
}

#' @export
supports_window_clause.DBIConnection <- function(con) {
  FALSE
}

#' @rdname db-sql
#' @export
db_supports_table_alias_with_as <- function(con) {
  UseMethod("db_supports_table_alias_with_as")
}

#' @export
db_supports_table_alias_with_as.DBIConnection <- function(con) {
  FALSE
}

#' @export
db_supports_table_alias_with_as.TestConnection <- function(con) {
  TRUE
}


# Query generation --------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_select <- function(
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
) {
  check_dots_used()
  UseMethod("sql_query_select")
}

#' @export
sql_query_select.DBIConnection <- function(
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
) {
  sql_select_clauses(
    con,
    select = sql_clause_select(con, select, distinct),
    from = sql_clause_from(from),
    where = sql_clause_where(where),
    group_by = sql_clause_group_by(group_by),
    having = sql_clause_having(having),
    window = sql_clause_window(window),
    order_by = sql_clause_order_by(order_by, subquery, limit),
    limit = sql_clause_limit(con, limit),
    lvl = lvl
  )
}
dbplyr_query_select <- function(
  con,
  select,
  from,
  where = NULL,
  group_by = NULL,
  having = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  ...
) {
  check_2ed(con)
  # TODO should add argument `window` after tidyverse/dplyr#4663
  # TODO should add argument `subquery` after tidyverse/dplyr#4663
  sql_query_select(
    con,
    select,
    from,
    where = where,
    group_by = group_by,
    having = having,
    order_by = order_by,
    limit = limit,
    distinct = distinct,
    ...
  )
}

#' @rdname db-sql
#' @export
sql_query_join <- function(
  con,
  x,
  y,
  select,
  type = "inner",
  by = NULL,
  na_matches = FALSE,
  ...,
  lvl = 0
) {
  check_dots_used()
  UseMethod("sql_query_join")
}
#' @export
sql_query_join.DBIConnection <- function(
  con,
  x,
  y,
  select,
  type = "inner",
  by = NULL,
  na_matches = FALSE,
  ...,
  lvl = 0
) {
  JOIN <- switch(
    type,
    left = sql("LEFT JOIN"),
    inner = sql("INNER JOIN"),
    right = sql("RIGHT JOIN"),
    full = sql("FULL JOIN"),
    cross = sql("CROSS JOIN"),
    cli_abort("Unknown join type: {.val {type}}")
  )

  x <- dbplyr_sql_subquery(con, x, name = by$x_as, lvl = lvl)
  y <- dbplyr_sql_subquery(con, y, name = by$y_as, lvl = lvl)

  on <- sql_join_tbls(con, by, na_matches = na_matches)

  # Wrap with SELECT since callers assume a valid query is returned
  clauses <- list(
    sql_clause_select(con, select),
    sql_clause_from(x),
    sql_clause(JOIN, y),
    sql_clause("ON", on, sep = " AND", parens = TRUE, lvl = 1)
  )
  sql_format_clauses(clauses, lvl, con)
}
dbplyr_query_join <- function(
  con,
  x,
  y,
  vars,
  type = "inner",
  by = NULL,
  na_matches = FALSE,
  ...,
  select = NULL,
  lvl = 0
) {
  check_2ed(con)
  sql_query_join(
    con,
    x,
    y,
    select,
    type = type,
    by = by,
    na_matches = na_matches,
    ...,
    lvl = lvl
  )
}

#' @rdname db-sql
#' @export
sql_query_multi_join <- function(
  con,
  x,
  joins,
  table_names,
  by_list,
  select,
  ...,
  lvl = 0
) {
  check_dots_used()
  UseMethod("sql_query_multi_join")
}

#' @export
#' @param vars tibble with six columns:
#'   * `table` `<tbl_lazy>`: the tables to join with.
#'   * `type` `<character>`: the join type (left, right, inner, full).
#'   * `by_x`, `by_y` `<list_of<character>>`: The columns to join by
#'   * `by_x_table_id` `<list_of<integer>>`: The table index where the join column
#'     comes from. This needs to be a list because a the join columns might come
#'     from different tables
#'   * `on` `<character>`
#'   * `na_matches` `<character>`: Either `"na"` or `"never"`.
#' @param select A named SQL vector.
#' @param table_names `<character>` The names of the tables.
#' @noRd
#' @examples
#' # Left join with *
#' df1 <- lazy_frame(x = 1, y = 1)
#' df2 <- lazy_frame(x = 1, z = 1)
#' df3 <- lazy_frame(x = 1, z2 = 1)
#'
#' tmp <- left_join(df1, df2, by = "x") |>
#'   left_join(df3, by = c("x", z = "z2"))
#' tibble(
#'   table = list(df1, df2),
#'   type = c("left", "left"),
#'   by_x = list("x", c("x", "z")),
#'   by_y = list("x", c("x", "z2")),
#'   by_x_table_id = list(1L, c(1L, 2L)),
#'   on = c(NA, NA),
#'   na_matches = c("never", "never")
#' )
sql_query_multi_join.DBIConnection <- function(
  con,
  x,
  joins,
  table_names,
  by_list,
  select,
  ...,
  lvl = 0
) {
  if (vctrs::vec_duplicate_any(table_names)) {
    cli_abort("{.arg table_names} must be unique.")
  }

  from <- dbplyr_sql_subquery(con, x, name = table_names[[1]], lvl = lvl)

  join_table_queries <- purrr::map2(
    joins$table,
    table_names[-1],
    function(table, name) {
      dbplyr_sql_subquery(con, table, name = name, lvl = lvl)
    }
  )
  types <- toupper(paste0(joins$type, " JOIN"))
  join_clauses <- purrr::map2(
    types,
    join_table_queries,
    \(join_kw, from) sql_clause(join_kw, from)
  )

  on_clauses <- purrr::map(
    joins$by,
    function(by) {
      on <- sql_join_tbls(con, by = by, na_matches = by$na_matches)
      sql_clause("ON", on, sep = " AND", parens = TRUE, lvl = 1)
    }
  )
  join_on_clauses <- vctrs::vec_interleave(join_clauses, on_clauses)

  clauses <- list2(
    sql_clause_select(con, select),
    sql_clause_from(from),
    !!!join_on_clauses
  )

  sql_format_clauses(clauses, lvl = lvl, con = con)
}

#' @rdname db-sql
#' @export
sql_query_semi_join <- function(
  con,
  x,
  y,
  anti,
  by,
  where,
  vars,
  ...,
  lvl = 0
) {
  check_dots_used()
  UseMethod("sql_query_semi_join")
}
#' @export
sql_query_semi_join.DBIConnection <- function(
  con,
  x,
  y,
  anti,
  by,
  where,
  vars,
  ...,
  lvl = 0
) {
  x <- dbplyr_sql_subquery(con, x, name = by$x_as)
  y <- dbplyr_sql_subquery(con, y, name = by$y_as)

  on <- sql_join_tbls(con, by, na_matches = by$na_matches)

  lines <- list(
    sql_clause_select(con, vars),
    sql_clause_from(x),
    glue_sql2(con, "WHERE ", if (anti) "NOT ", "EXISTS ("),
    # lvl = 1 because they are basically in a subquery
    sql_clause("SELECT 1 FROM", y, lvl = 1),
    sql_clause_where(c(on, where), lvl = 1),
    sql(")")
  )
  sql_format_clauses(lines, lvl, con)
}

dbplyr_query_semi_join <- function(
  con,
  x,
  y,
  anti = FALSE,
  by = NULL,
  ...,
  lvl = 0
) {
  check_2ed(con)
  sql_query_semi_join(con, x, y, anti = anti, by = by, ..., lvl = lvl)
}

#' @rdname db-sql
#' @export
sql_query_set_op <- function(con, x, y, method, ..., all = FALSE, lvl = 0) {
  check_dots_used()
  UseMethod("sql_query_set_op")
}
#' @export
sql_query_set_op.DBIConnection <- function(
  con,
  x,
  y,
  method,
  ...,
  all = FALSE,
  lvl = 0
) {
  method <- paste0(method, if (all) " ALL")
  method <- style_kw(method)
  lines <- list(
    sql_indent_subquery(x, con = con, lvl = lvl),
    sql(method),
    sql_indent_subquery(y, con = con, lvl = lvl)
  )
  sql_format_clauses(lines, lvl, con)
}

#' @rdname db-sql
#' @export
sql_query_union <- function(con, x, unions, ..., lvl = 0) {
  UseMethod("sql_query_union")
}
#' @export
sql_query_union.DBIConnection <- function(con, x, unions, ..., lvl = 0) {
  methods <- ifelse(unions$all, "UNION ALL", "UNION")
  methods <- indent_lvl(style_kw(methods), lvl)
  tables <- unlist(unions$table)

  union_clauses <- vctrs::vec_interleave(as.character(methods), tables)
  out <- paste0(
    x,
    "\n\n",
    paste0(union_clauses, collapse = "\n\n")
  )

  sql(out)
}

#' Generate SQL for Insert, Update, Upsert, and Delete
#'
#' These functions generate the SQL used in `rows_*(in_place = TRUE)`.
#'
#' @param con Database connection.
#' @param table Table to update. Must be a table identifier.
#'   Use a string to refer to tables in the current schema/catalog or
#'   `I()` to refer to tables in other schemas/catalogs.
#' @param from Table or query that contains the new data. Either a table
#'   identifier or SQL.
#' @inheritParams dplyr::rows_upsert
#' @param insert_cols Names of columns to insert.
#' @param update_cols Names of columns to update.
#' @param update_values A named SQL vector that specify how to update the columns.
#' @param ... Other parameters passed onto methods.
#' @param returning_cols Optional. Names of columns to return.
#' @param method Optional. The method to use.
#'
#' @details Insert Methods
#' ## `"where_not_exists"`
#' The default for most databases.
#'
#' ```
#' INSERT INTO x_name
#' SELECT *
#' FROM y
#' WHERE NOT EXISTS <match on by columns>
#' ```
#'
#' ## `"on_conflict"`
#' Supported by:
#' * Postgres
#' * SQLite
#'
#' This method uses the `ON CONFLICT` clause and therefore requires a unique
#' index on the columns specified in `by`.
#'
#' @details Upsert Methods
#'
#' ## `"merge"`
#' The upsert method according to the SQL standard. It uses the `MERGE` statement
#'
#' ```
#' MERGE INTO x_name
#' USING y
#'   ON <match on by columns>
#' WHEN MATCHED THEN
#'   UPDATE SET ...
#' WHEN NOT MATCHED THEN
#'   INSERT ...
#' ```
#'
#' ## `"on_conflict"`
#' Supported by:
#' * Postgres
#' * SQLite
#'
#' This method uses the `ON CONFLICT` clause and therefore requires a unique
#' index on the columns specified in `by`.
#'
#' ## `"cte_update"`
#' Supported by:
#' * Postgres
#' * SQLite
#' * Oracle
#'
#' The classical way to upsert in Postgres and SQLite before support for
#' `ON CONFLICT` was added. The update is done in a CTE clause and the unmatched
#' values are then inserted outside of the CTE.
#'
#' @return A SQL query.
#' @export
#'
#' @examples
#' sql_query_upsert(
#'   con = simulate_postgres(),
#'   table = ident("airlines"),
#'   from = ident("df"),
#'   by = "carrier",
#'   update_cols = "name"
#' )
sql_query_insert <- function(
  con,
  table,
  from,
  insert_cols,
  by,
  ...,
  conflict = c("error", "ignore"),
  returning_cols = NULL,
  method = NULL
) {
  check_table_id(table)
  check_table_source(from)
  check_character(insert_cols)
  check_character(by)
  check_character(returning_cols, allow_null = TRUE)

  check_dots_used()
  UseMethod("sql_query_insert")
}

#' @export
sql_query_insert.DBIConnection <- function(
  con,
  table,
  from,
  insert_cols,
  by,
  ...,
  conflict = c("error", "ignore"),
  returning_cols = NULL,
  method = NULL
) {
  table <- as_table_path(table, con)
  from <- as_table_source(from, con)

  method <- method %||% "where_not_exists"
  arg_match(method, "where_not_exists", error_arg = "method")
  # https://stackoverflow.com/questions/25969/insert-into-values-select-from
  conflict <- rows_check_conflict(conflict)

  parts <- rows_insert_prep(con, table, from, insert_cols, by, lvl = 0)

  clauses <- list2(
    parts$insert_clause,
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    !!!parts$conflict_clauses,
    sql_returning_cols(con, returning_cols, table)
  )

  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname sql_query_insert
sql_query_append <- function(
  con,
  table,
  from,
  insert_cols,
  ...,
  returning_cols = NULL
) {
  if (is_tbl_lazy(from)) {
    lifecycle::deprecate_warn(
      when = "2.3.2",
      what = "sql_query_append(from = 'must be a table identifier or an SQL query, not a lazy table.')"
    )

    insert_cols <- colnames(from)
    from <- sql_render(from, con = con, lvl = 1)
    out <- sql_query_append(
      con = con,
      table = table,
      from = from,
      insert_cols = insert_cols,
      returning_cols = returning_cols
    )

    return(out)
  }

  check_table_id(table)
  check_table_source(from)
  check_character(insert_cols)
  check_character(returning_cols, allow_null = TRUE)

  check_dots_used()
  UseMethod("sql_query_append")
}

#' @export
sql_query_append.DBIConnection <- function(
  con,
  table,
  from,
  insert_cols,
  ...,
  returning_cols = NULL
) {
  table <- as_table_path(table, con)
  from <- as_table_source(from, con)

  # https://stackoverflow.com/questions/25969/insert-into-values-select-from
  parts <- rows_prep(con, table, from, by = list(), lvl = 0)
  insert_cols <- escape(
    ident(insert_cols),
    collapse = ", ",
    parens = TRUE,
    con = con
  )

  clauses <- list2(
    sql_clause_insert(con, insert_cols, table),
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    sql_returning_cols(con, returning_cols, table)
  )

  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname sql_query_insert
sql_query_update_from <- function(
  con,
  table,
  from,
  by,
  update_values,
  ...,
  returning_cols = NULL
) {
  check_table_id(table)
  check_table_source(from)
  check_character(by)
  check_character(update_values)
  check_named(update_values)
  check_character(returning_cols, allow_null = TRUE)

  check_dots_used()
  UseMethod("sql_query_update_from")
}

#' @export
sql_query_update_from.DBIConnection <- function(
  con,
  table,
  from,
  by,
  update_values,
  ...,
  returning_cols = NULL
) {
  table <- as_table_path(table, con)
  from <- as_table_source(from, con)

  # https://stackoverflow.com/questions/2334712/how-do-i-update-from-a-select-in-sql-server
  parts <- rows_prep(con, table, from, by, lvl = 0)
  update_cols <- sql_escape_ident(con, names(update_values))

  # avoid CTEs for the general case as they do not work everywhere
  clauses <- list(
    sql_clause_update(table),
    sql_clause_set(update_cols, update_values),
    sql_clause_from(parts$from),
    sql_clause_where(parts$where),
    sql_returning_cols(con, returning_cols, table)
  )
  sql_format_clauses(clauses, lvl = 0, con)
}


#' @export
#' @rdname sql_query_insert
sql_query_upsert <- function(
  con,
  table,
  from,
  by,
  update_cols,
  ...,
  returning_cols = NULL,
  method = NULL
) {
  check_table_id(table)
  check_table_source(from)
  check_character(by)
  check_character(update_cols)
  check_character(returning_cols, allow_null = TRUE)

  # https://wiki.postgresql.org/wiki/UPSERT#SQL_MERGE_syntax
  # https://github.com/cynkra/dm/pull/616#issuecomment-920613435
  check_dots_used()
  UseMethod("sql_query_upsert")
}

#' @export
sql_query_upsert.DBIConnection <- function(
  con,
  table,
  from,
  by,
  update_cols,
  ...,
  returning_cols = NULL,
  method = NULL
) {
  table <- as_table_path(table, con)
  from <- as_table_source(from, con)

  method <- method %||% "cte_update"
  arg_match(method, "cte_update", error_arg = "method")

  parts <- rows_prep(con, table, from, by, lvl = 0)

  insert_cols <- c(by, update_cols)
  insert_cols <- escape(
    ident(insert_cols),
    collapse = ", ",
    parens = TRUE,
    con = con
  )

  update_values <- sql_table_prefix(con, update_cols, "...y")
  update_cols <- sql_escape_ident(con, update_cols)

  updated_cte <- list(
    sql_clause_update(table),
    sql_clause_set(update_cols, update_values),
    sql_clause_from(parts$from),
    sql_clause_where(parts$where),
    sql(paste0("RETURNING ", sql_star(con, table)))
  )
  updated_sql <- sql_format_clauses(updated_cte, lvl = 1, con)
  update_name <- sql(escape(ident("updated"), con = con))

  join_by <- list(
    x = by,
    y = by,
    x_as = "updated",
    y_as = "...y",
    condition = "="
  )
  where <- sql_join_tbls(con, by = join_by, na_matches = "never")

  clauses <- list2(
    sql(paste0("WITH ", update_name, " AS (")),
    updated_sql,
    sql(")"),
    sql_clause_insert(con, insert_cols, table),
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    !!!sql_clause_where_exists(update_name, where, not = TRUE),
    sql_returning_cols(con, returning_cols, table)
  )

  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname sql_query_insert
sql_query_delete <- function(con, table, from, by, ..., returning_cols = NULL) {
  check_table_id(table)
  check_table_source(from)
  check_character(by)
  check_character(returning_cols, allow_null = TRUE)

  check_dots_used()
  UseMethod("sql_query_delete")
}

#' @export
sql_query_delete.DBIConnection <- function(
  con,
  table,
  from,
  by,
  ...,
  returning_cols = NULL
) {
  table <- as_table_path(table, con)
  from <- as_table_source(from, con)
  parts <- rows_prep(con, table, from, by, lvl = 1)

  clauses <- list2(
    sql_clause("DELETE FROM", table),
    !!!sql_clause_where_exists(parts$from, parts$where, not = FALSE),
    sql_returning_cols(con, returning_cols, table)
  )
  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname db-sql
sql_returning_cols <- function(con, cols, table, ...) {
  if (is_empty(cols)) {
    return(NULL)
  }

  check_dots_used()
  UseMethod("sql_returning_cols")
}

#' @export
sql_returning_cols.DBIConnection <- function(con, cols, table, ...) {
  returning_cols <- sql_named_cols(con, cols, table = table)

  sql_clause("RETURNING", returning_cols)
}

sql_named_cols <- function(con, cols, table = NULL) {
  nms <- names2(cols)
  nms[nms == cols] <- ""

  cols <- sql_table_prefix(con, cols, table)
  cols <- set_names(ident_q(cols), nms)
  escape(cols, collapse = NULL, con = con)
}

# dplyr fallbacks ---------------------------------------------------------

dbplyr_analyze <- function(con, table, ...) {
  check_2ed(con)

  sql <- sql_table_analyze(con, table, ...)
  if (is.null(sql)) {
    return() # nocov
  }

  db_execute(
    con,
    sql,
    "Can't analyze table {.field {format(table, con = con)}}."
  )
}

dbplyr_create_index <- function(
  con,
  table,
  columns,
  name = NULL,
  unique = FALSE,
  ...
) {
  check_2ed(con)
  sql <- sql_table_index(con, table, columns, name = name, unique = unique, ...)
  db_execute(
    con,
    sql,
    "Can't create index on table {.field {format(table, con = con)}}."
  )
}

dbplyr_explain <- function(con, sql, ...) {
  check_2ed(con)

  sql <- sql_query_explain(con, sql, ...)
  n <- length(sql)
  for (i in seq_len(n - 1)) {
    db_execute(con, sql[[i]], "Can't explain query.")
  }
  expl <- db_get_query(con, sql[[n]], "Can't explain query.")

  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}


dbplyr_query_fields <- function(con, sql, ...) {
  check_2ed(con)

  sql <- sql_query_fields(con, sql, ...)
  df <- db_get_query(con, sql, "Can't query fields.")
  names(df)
}

dbplyr_save_query <- function(
  con,
  sql,
  name,
  temporary = TRUE,
  ...,
  overwrite = FALSE
) {
  check_2ed(con)

  name <- as_table_path(name, con)
  sql <- sql_query_save(con, sql, name, temporary = temporary, ...)

  if (overwrite) {
    found <- DBI::dbExistsTable(con, SQL(name))
    if (found) {
      DBI::dbRemoveTable(con, SQL(name))
    }
  }

  db_execute(
    con,
    sql,
    "Can't save query to table {.table {format(name, con = con)}}."
  )

  name
}

dbplyr_sql_subquery <- function(
  con,
  from,
  name = unique_subquery_name(),
  ...,
  lvl = 0
) {
  check_2ed(con)
  sql_query_wrap(con, from = from, name = name, ..., lvl = lvl)
}

# Helpers -------------------------------------------------------------------

db_execute <- function(con, sql, msg, call = caller_env(), env = caller_env()) {
  dbi_wrap(
    dbExecute(con, sql, immediate = TRUE),
    sql = sql,
    msg = msg,
    call = call,
    env = env
  )
  invisible()
}

db_get_query <- function(
  con,
  sql,
  msg,
  call = caller_env(),
  env = caller_env()
) {
  dbi_wrap(dbGetQuery(con, sql), sql, msg, call = call, env = env)
}

dbi_wrap <- function(code, sql, msg, call = caller_env(), env = caller_env()) {
  withCallingHandlers(
    code,
    error = function(cnd) {
      msg <- c(msg, i = paste0("Using SQL: ", sql))
      cli_abort(msg, parent = cnd, call = call, .envir = env)
    }
  )
}
