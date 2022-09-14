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
#' * `sql_random(con)` generates SQL to get a random number which can be used
#'   to select random rows in `slice_sample()`.
#'
#' * `supports_window_clause(con)` does the backend support named windows?
#'
#' * `supports_star_without_alias(con)` does the backend support using `*`
#'   in a `SELECT` query without prefixing by a table alias?
#'
#' Tables:
#'
#' * `sql_table_analyze(con, table)` generates SQL that "analyzes" the table,
#'   ensuring that the database has up-to-date statistics for use in the query
#'   planner. It called from [copy_to()] when `analyze = TRUE`.
#'
#' * `sql_table_index()` generates SQL for adding an index to table. The
#'
#' Query manipulation:
#'
#' * `sql_query_explain(con, sql)` generates SQL that "explains" a query,
#'   i.e. generates a query plan describing what indexes etc that the
#'   database will use.
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
#' @section dbplyr 2.0.0:
#'
#' Many `dplyr::db_*` generics have been replaced by `dbplyr::sql_*` generics.
#' To update your backend, you'll need to extract the SQL generation out of your
#' existing code, and place it in a new method for a dbplyr `sql_` generic.
#'
#' * `dplyr::db_analyze()` is replaced by `dbplyr::sql_table_analyze()`
#' * `dplyr::db_explain()` is replaced by `dbplyr::sql_query_explain()`
#' * `dplyr::db_create_index()` is replaced by `dbplyr::sql_table_index()`
#' * `dplyr::db_query_fields()` is replaced by `dbplyr::sql_query_fields()`
#' * `dplyr::db_query_rows()` is no longer used; you can delete it
#' * `dplyr::db_save_query()` is replaced by `dbplyr::sql_query_save()`
#'
#' The query generating functions have also changed names. Their behaviour is
#' unchanged, so you just need to rename the generic and import from dbplyr
#' instead of dplyr.
#'
#' * `dplyr::sql_select()` is replaced by `dbplyr::sql_query_select()`
#' * `dplyr::sql_join()` is replaced by `dbplyr::sql_query_join()`
#' * `dplyr::sql_semi_join()` is replaced by `dbplyr::sql_query_semi_join()`
#' * `dplyr::sql_set_op()` is replaced by `dbplyr::sql_query_set_op()`
#' * `dplyr::sql_subquery()` is replaced by `dbplyr::sql_query_wrap()`
#'
#' Learn more in `vignette("backend-2.0")`
#'
#' @keywords internal
#' @family generic
#' @name db-sql
NULL

#' @export
#' @rdname db-sql
sql_expr_matches <- function(con, x, y) {
  UseMethod("sql_expr_matches")
}
# https://modern-sql.com/feature/is-distinct-from
#' @export
sql_expr_matches.DBIConnection <- function(con, x, y) {
  build_sql(
    "CASE WHEN (", x, " = ", y, ") OR (", x, " IS NULL AND ", y, " IS NULL) ",
    "THEN 0 ",
    "ELSE 1 ",
    "END = 0",
    con = con
  )
}

#' @export
#' @rdname db-sql
sql_translation <- function(con) {
  UseMethod("sql_translation")
}
# sql_translation.DBIConnection lives in backend-.R
dbplyr_sql_translation <- function(con) {
  dbplyr_fallback(con, "sql_translate_env")
}
#' @importFrom dplyr sql_translate_env
#' @export
sql_translate_env.DBIConnection <- function(con) {
  sql_translation(con)
}

#' @export
#' @rdname db-sql
sql_random <- function(con) {
  UseMethod("sql_random")
}


# Tables ------------------------------------------------------------------

#' @rdname db-sql
#' @export
sql_table_analyze <- function(con, table, ...) {
  UseMethod("sql_table_analyze")
}
#' @export
sql_table_analyze.DBIConnection <- function(con, table, ...) {
  build_sql("ANALYZE ", as.sql(table, con = con), con = con)
}

#' @rdname db-sql
#' @export
sql_table_index <- function(con, table, columns, name = NULL, unique = FALSE, ...) {
  UseMethod("sql_table_index")
}
#' @export
sql_table_index.DBIConnection <- function(con, table, columns, name = NULL,
                                           unique = FALSE, ...) {
  assert_that(is_string(table) || is_schema(table), is.character(columns))

  name <- name %||% paste0(c(unclass(table), columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  build_sql(
    "CREATE ", if (unique) sql("UNIQUE "), "INDEX ", as.sql(name, con = con),
    " ON ", as.sql(table, con = con), " ", fields,
    con = con
  )
}

# Query manipulation ------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_explain <- function(con, sql, ...) {
  UseMethod("sql_query_explain")
}
#' @export
sql_query_explain.DBIConnection <- function(con, sql, ...) {
  build_sql("EXPLAIN ", sql, con = con)
}

#' @rdname db-sql
#' @export
sql_query_fields <- function(con, sql, ...) {
  UseMethod("sql_query_fields")
}
#' @export
sql_query_fields.DBIConnection <- function(con, sql, ...) {
  dbplyr_query_select(con, sql("*"), dbplyr_sql_subquery(con, sql), where = sql("0 = 1"))
}

#' @rdname db-sql
#' @export
sql_query_save <- function(con, sql, name, temporary = TRUE, ...) {
  UseMethod("sql_query_save")
}
#' @export
sql_query_save.DBIConnection <- function(con, sql, name, temporary = TRUE, ...) {
  build_sql(
    "CREATE ", if (temporary) sql("TEMPORARY "), "TABLE \n",
    as.sql(name, con), " AS\n", sql,
    con = con
  )
}
#' @export
#' @rdname db-sql
sql_query_wrap <- function(con, from, name = NULL, ..., lvl = 0) {
  UseMethod("sql_query_wrap")
}
#' @export
sql_query_wrap.DBIConnection <- function(con, from, name = NULL, ..., lvl = 0) {
  if (is.ident(from)) {
    setNames(from, name)
  } else if (is_schema(from) || is_catalog(from)) {
    setNames(as.sql(from, con), name)
  } else {
    build_sql(sql_indent_subquery(from, con, lvl), " ", as_subquery_name(name), con = con)
  }
}

as_subquery_name <- function(x, default = ident(unique_subquery_name())) {
  if (is.ident(x)) {
    x
  } else if (is.null(x)) {
    default
  } else {
    ident(x)
  }
}

#' @export
#' @rdname db-sql
sql_indent_subquery <- function(from, con, lvl = 0) {
  multi_line <- grepl(x = from, pattern = "\\r\\n|\\r|\\n")
  if (multi_line) {
    build_sql(
      "(\n",
      from, "\n",
      indent_lvl(")", lvl),
      con = con
    )
  } else {
    # Strip indent
    from <- gsub("^ +", "", from)
    build_sql("(", from, ")", con = con)
  }
}

#' @rdname db-sql
#' @export
sql_query_rows <- function(con, sql, ...) {
  UseMethod("sql_query_rows")
}
#' @export
sql_query_rows.DBIConnection <- function(con, sql, ...) {
  from <- dbplyr_sql_subquery(con, sql, "master")
  build_sql("SELECT COUNT(*) FROM ", from, con = con)
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
supports_star_without_alias <- function(con) {
  UseMethod("supports_star_without_alias")
}

#' @export
supports_star_without_alias.DBIConnection <- function(con) {
  TRUE
}


# Query generation --------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_select <- function(con, select, from, where = NULL,
                             group_by = NULL, having = NULL,
                             window = NULL,
                             order_by = NULL,
                             limit = NULL,
                             distinct = FALSE,
                             ...,
                             subquery = FALSE,
                             lvl = 0) {
  UseMethod("sql_query_select")
}

#' @export
sql_query_select.DBIConnection <- function(con, select, from, where = NULL,
                               group_by = NULL, having = NULL,
                               window = NULL,
                               order_by = NULL,
                               limit = NULL,
                               distinct = FALSE,
                               ...,
                               subquery = FALSE,
                               lvl = 0) {
  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct),
    from      = sql_clause_from(from),
    where     = sql_clause_where(where),
    group_by  = sql_clause_group_by(group_by),
    having    = sql_clause_having(having),
    window    = sql_clause_window(window),
    order_by  = sql_clause_order_by(order_by, subquery, limit),
    limit     = sql_clause_limit(con, limit),
    lvl       = lvl
  )
}
dbplyr_query_select <- function(con, ...) {
  dbplyr_fallback(con, "sql_select", ...)
}
#' @importFrom dplyr sql_select
#' @export
sql_select.DBIConnection <- function(con, select, from, where = NULL,
                                     group_by = NULL, having = NULL,
                                     window = NULL,
                                     order_by = NULL,
                                     limit = NULL,
                                     distinct = FALSE,
                                     ...,
                                     subquery = FALSE) {
  sql_query_select(
    con, select, from,
    where = where,
    group_by = group_by,
    having = having,
    window = window,
    order_by = order_by,
    limit = limit,
    distinct = distinct,
    ...,
    subquery = subquery
  )
}

#' @rdname db-sql
#' @export
sql_query_join <- function(con, x, y, vars, type = "inner", by = NULL, na_matches = FALSE, ..., lvl = 0) {
  UseMethod("sql_query_join")
}
#' @export
sql_query_join.DBIConnection <- function(con, x, y, vars, type = "inner", by = NULL, na_matches = FALSE, ..., lvl = 0) {
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

  select <- sql_join_vars(con, vars, x_as = by$x_as, y_as = by$y_as, type = type)
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
dbplyr_query_join <- function(con, ..., lvl = 0) {
  dbplyr_fallback(con, "sql_join", ..., lvl = lvl)
}
#' @export
#' @importFrom dplyr sql_join
sql_join.DBIConnection <- function(con, x, y, vars, type = "inner", by = NULL, na_matches = FALSE, ..., lvl = 0) {
  sql_query_join(
    con, x, y, vars,
    type = type,
    by = by,
    na_matches = na_matches,
    ...,
    lvl = lvl
  )
}

#' @rdname db-sql
#' @export
sql_query_multi_join <- function(con,
                                 x,
                                 joins,
                                 table_names,
                                 vars,
                                 all_vars_list,
                                 ...,
                                 lvl = 0) {
  UseMethod("sql_query_multi_join")
}

#' @export
sql_query_multi_join.DBIConnection <- function(con,
                                               x,
                                               joins,
                                               table_names,
                                               vars,
                                               all_vars_list,
                                               ...,
                                               lvl = 0) {
  if (vctrs::vec_duplicate_any(table_names)) {
    cli_abort("{.arg table_names} must be unique.")
  }

  x_name <- ident(table_names[[1]])

  select_sql <- sql_multi_join_vars(con, vars, table_names, all_vars_list)

  ons <- purrr::pmap(
    vctrs::vec_cbind(
      rhs = table_names[-1],
      joins[c("by_x", "by_x_table_id", "by_y", "on", "na_matches")]
    ),
    function(rhs, by_x, by_x_table_id, by_y, on, na_matches) {
      if (!is.na(on)) {
        return(sql(on))
      }

      by <- list(
        x = ident(by_x),
        y = ident(by_y),
        x_as = ident(table_names[by_x_table_id]),
        y_as = ident(rhs)
      )

      sql_join_tbls(con, by = by, na_matches = na_matches)
    }
  )

  from <- dbplyr_sql_subquery(con, x, name = table_names[[1]], lvl = lvl)
  join_table_queries <- purrr::map2(
    joins$table,
    table_names[-1],
    ~ dbplyr_sql_subquery(con, .x, name = .y, lvl = lvl)
  )
  types <- toupper(paste0(joins$type, " JOIN"))
  join_clauses <- vctrs::vec_interleave(
    purrr::map2(join_table_queries, types, ~ sql_clause(.y, .x)),
    purrr::map(ons, ~ sql_clause("ON", .x, sep = " AND", parens = TRUE, lvl = 1))
  )

  list2(
    sql_clause_select(con, select_sql),
    sql_clause_from(from),
    !!!join_clauses
  ) %>%
    sql_format_clauses(lvl = lvl, con = con)
}

sql_multi_join_vars <- function(con, vars, table_names, all_vars_list) {
  all_vars <- tolower(unlist(all_vars_list))
  duplicated_vars <- all_vars[vctrs::vec_duplicate_detect(all_vars)]
  duplicated_vars <- unique(duplicated_vars)

  # FIXME vectorise `sql_table_prefix()` (need to update `ident()` and friends for this...)
  ns <- vctrs::list_sizes(vars$table)
  if (any(ns > 1)) {
    # special treatment for `full_join()`
    out <- purrr::map2(
      vars$table, vars$var,
      function(table_ids, vars) {
        if (length(table_ids) > 1) {
          sql_expr(
            COALESCE(
              !!sql_table_prefix(con, vars[[1]], table = ident(table_names[[1]])),
              !!sql_table_prefix(con, vars[[2]], table = ident(table_names[[2]]))
            ),
            con = con
          )
        } else {
          table_id <- table_ids[[1]]
          var <- vars[[1]]
          sql_multi_join_var(con, var, table_id, table_names, duplicated_vars)
        }
      }
    )

    out <- set_names(out, vars$name)
    return(sql(unlist(out)))
  }

  out <- rep_named(vars$name, list())
  vars$var <- vctrs::vec_unchop(vars$var)
  vars$table <- vctrs::vec_unchop(vars$table)

  for (i in seq_along(table_names)) {
    all_vars_current <- all_vars_list[[i]]
    vars_idx <- which(vars$table == i)
    used_vars_current <- vars$var[vars_idx]
    out_vars_current <- vars$name[vars_idx]

    if (join_can_use_star(all_vars_current, used_vars_current, out_vars_current, vars_idx)) {
      id <- vars_idx[[1]]
      tbl_alias <- escape(ident(table_names[i]), con = con)
      out[[id]] <- sql(paste0(tbl_alias, ".*"))
      names(out)[id] <- ""
    } else {
      out[vars_idx] <- purrr::map2(
        used_vars_current, i,
        ~ sql_multi_join_var(con, .x, .y, table_names, duplicated_vars)
      )

    }
  }

  out <- purrr::compact(out)
  sql(unlist(out))
}

join_can_use_star <- function(all_vars, used_vars, out_vars, idx) {
  # using `tbl.*` for a single variable is silly
  if (length(all_vars) <= 1) {
    return(FALSE)
  }

  # all variables need to be used
  if (!identical(used_vars, all_vars)) {
    return(FALSE)
  }

  # they must not be renamed
  if (!identical(used_vars, out_vars)) {
    return(FALSE)
  }

  # the variables must form a sequence
  all(diff(idx) == 1)
}

sql_multi_join_var <- function(con, var, table_id, table_names, duplicated_vars) {
  if (tolower(var) %in% duplicated_vars) {
    sql_table_prefix(con, var, ident(table_names[[table_id]]))
  } else {
    sql_escape_ident(con, var)
  }
}

#' @rdname db-sql
#' @export
sql_query_semi_join <- function(con, x, y, anti, by, vars, ..., lvl = 0) {
  UseMethod("sql_query_semi_join")
}
#' @export
sql_query_semi_join.DBIConnection <- function(con, x, y, anti, by, vars, ..., lvl = 0) {
  x <- dbplyr_sql_subquery(con, x, name = by$x_as)
  y <- dbplyr_sql_subquery(con, y, name = by$y_as)

  on <- sql_join_tbls(con, by)

  lines <- list(
    sql_clause_select(con, vars),
    sql_clause_from(x),
    build_sql("WHERE ", if (anti) sql("NOT "), "EXISTS (", con = con),
    # lvl = 1 because they are basically in a subquery
    sql_clause("SELECT 1 FROM", y, lvl = 1),
    # don't use `sql_clause_where()` to avoid wrapping each element in parens
    sql_clause("WHERE", on, sep = " AND", parens = TRUE, lvl = 1),
    sql(")")
  )
  sql_format_clauses(lines, lvl, con)
}

dbplyr_query_semi_join <- function(con, ...) {
  dbplyr_fallback(con, "sql_semi_join", ...)
}
#' @export
#' @importFrom dplyr sql_semi_join
sql_semi_join.DBIConnection <- function(con, x, y, anti = FALSE, by = NULL, ..., lvl = 0) {
  sql_query_semi_join(con, x, y, anti = anti, by = by, ..., lvl = lvl)
}

#' @rdname db-sql
#' @export
sql_query_set_op <- function(con, x, y, method, ..., all = FALSE, lvl = 0) {
  UseMethod("sql_query_set_op")
}
#' @export
sql_query_set_op.DBIConnection <- function(con, x, y, method, ..., all = FALSE, lvl = 0) {
  method <- paste0(method, if (all) " ALL")
  method <- style_kw(method)
  lines <- list(
    sql_indent_subquery(x, con = con, lvl = lvl),
    sql(method),
    sql_indent_subquery(y, con = con, lvl = lvl)
  )
  sql_format_clauses(lines, lvl, con)
}
# nocov start
dbplyr_query_set_op <- function(con, ...) {
  dbplyr_fallback(con, "sql_set_op", ...)
}
#' @importFrom dplyr sql_set_op
#' @export
sql_set_op.DBIConnection <- function(con, x, y, method) {
  # dplyr::sql_set_op() doesn't have ...
  sql_query_set_op(con, x, y, method)
}
# nocov end

#' Generate SQL for Insert, Update, Upsert, and Delete
#'
#' These functions generate the SQL used in `rows_*(in_place = TRUE)`.
#'
#' @param con Database connection.
#' @param x_name Name of the table to update.
#' @param y A lazy tbl.
#' @inheritParams dplyr::rows_upsert
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
#' lf <- lazy_frame(
#'   carrier = c("9E", "AA"),
#'   name = c("Endeavor Air Inc.", "American Airlines Inc."),
#'   con = simulate_postgres()
#' )
#'
#' sql_query_upsert(
#'   simulate_postgres(),
#'   ident("airlines"),
#'   lf,
#'   by = "carrier",
#'   update_cols = "name"
#' )
sql_query_insert <- function(con,
                             x_name,
                             y,
                             by,
                             ...,
                             conflict = c("error", "ignore"),
                             returning_cols = NULL,
                             method = NULL) {
  rlang::check_dots_used()
  UseMethod("sql_query_insert")
}

#' @export
sql_query_insert.DBIConnection <- function(con,
                                           x_name,
                                           y,
                                           by,
                                           ...,
                                           conflict = c("error", "ignore"),
                                           returning_cols = NULL,
                                           method = NULL) {
  method <- method %||% "where_not_exists"
  arg_match(method, "where_not_exists", error_arg = "method")
  # https://stackoverflow.com/questions/25969/insert-into-values-select-from
  conflict <- rows_check_conflict(conflict)

  parts <- rows_insert_prep(con, x_name, y, by, lvl = 0)

  clauses <- list2(
    parts$insert_clause,
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    !!!parts$conflict_clauses,
    sql_returning_cols(con, returning_cols, x_name)
  )

  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname sql_query_insert
sql_query_append <- function(con, x_name, y, ..., returning_cols = NULL) {
  rlang::check_dots_used()
  UseMethod("sql_query_append")
}

#' @export
sql_query_append.DBIConnection <- function(con, x_name, y, ..., returning_cols = NULL) {
  # https://stackoverflow.com/questions/25969/insert-into-values-select-from
  parts <- rows_prep(con, x_name, y, by = list(), lvl = 0)
  insert_cols <- escape(ident(colnames(y)), collapse = ", ", parens = TRUE, con = con)

  clauses <- list2(
    sql_clause_insert(con, insert_cols, x_name),
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    sql_returning_cols(con, returning_cols, x_name)
  )

  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname sql_query_insert
sql_query_update_from <- function(con, x_name, y, by, update_values, ...,
                                  returning_cols = NULL) {
  rlang::check_dots_used()
  UseMethod("sql_query_update_from")
}

#' @export
sql_query_update_from.DBIConnection <- function(con, x_name, y, by,
                                                update_values, ...,
                                                returning_cols = NULL) {
  # https://stackoverflow.com/questions/2334712/how-do-i-update-from-a-select-in-sql-server
  parts <- rows_prep(con, x_name, y, by, lvl = 0)
  update_cols <- sql_escape_ident(con, names(update_values))

  # avoid CTEs for the general case as they do not work everywhere
  clauses <- list(
    sql_clause_update(x_name),
    sql_clause_set(update_cols, update_values),
    sql_clause_from(parts$from),
    sql_clause_where(parts$where),
    sql_returning_cols(con, returning_cols, x_name)
  )
  sql_format_clauses(clauses, lvl = 0, con)
}


#' @export
#' @rdname sql_query_insert
sql_query_upsert <- function(con,
                             x_name,
                             y,
                             by,
                             update_cols,
                             ...,
                             returning_cols = NULL,
                             method = NULL) {
  # https://wiki.postgresql.org/wiki/UPSERT#SQL_MERGE_syntax
  # https://github.com/cynkra/dm/pull/616#issuecomment-920613435
  rlang::check_dots_used()
  UseMethod("sql_query_upsert")
}

#' @export
sql_query_upsert.DBIConnection <- function(con,
                                           x_name,
                                           y,
                                           by,
                                           update_cols,
                                           ...,
                                           returning_cols = NULL,
                                           method = NULL) {
  method <- method %||% "cte_update"
  arg_match(method, "cte_update", error_arg = "method")

  parts <- rows_prep(con, x_name, y, by, lvl = 0)

  update_values <- sql_table_prefix(con, update_cols, ident("...y"))
  update_cols <- sql_escape_ident(con, update_cols)

  updated_cte <- list(
    sql_clause_update(x_name),
    sql_clause_set(update_cols, update_values),
    sql_clause_from(parts$from),
    sql_clause_where(parts$where),
    sql(paste0("RETURNING ", escape(x_name, con = con), ".*"))
  )
  updated_sql <- sql_format_clauses(updated_cte, lvl = 1, con)
  update_name <- sql(escape(ident("updated"), con = con))

  join_by <- list(x = by, y = by, x_as = ident("updated"), y_as = ident("...y"))
  where <- sql_join_tbls(con, by = join_by, na_matches = "never")

  insert_cols <- escape(ident(colnames(y)), collapse = ", ", parens = TRUE, con = con)
  clauses <- list2(
    sql(paste0("WITH ", update_name, " AS (")),
    updated_sql,
    sql(")"),
    sql_clause_insert(con, insert_cols, x_name),
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    !!!sql_clause_where_exists(update_name, where, not = TRUE),
    sql_returning_cols(con, returning_cols, x_name)
  )

  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname sql_query_insert
sql_query_delete <- function(con, x_name, y, by, ..., returning_cols = NULL) {
  rlang::check_dots_used()
  UseMethod("sql_query_delete")
}

#' @export
sql_query_delete.DBIConnection <- function(con, x_name, y, by, ..., returning_cols = NULL) {
  parts <- rows_prep(con, x_name, y, by, lvl = 0)

  clauses <- list2(
    sql_clause("DELETE FROM", x_name),
    !!!sql_clause_where_exists(parts$from, parts$where, not = FALSE),
    sql_returning_cols(con, returning_cols, x_name)
  )
  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
#' @rdname db-sql
sql_returning_cols <- function(con, cols, table, ...) {
  if (is_empty(cols)) {
    return(NULL)
  }

  rlang::check_dots_empty()
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

dbplyr_analyze <- function(con, ...) {
  dbplyr_fallback(con, "db_analyze", ...)
}
#' @export
#' @importFrom dplyr db_analyze
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- sql_table_analyze(con, table, ...)
  if (is.null(sql)) {
    return() # nocov
  }
  tryCatch(
    DBI::dbExecute(con, sql),
    error = function(cnd) {
      msg <- "Can't analyze table {.val {table}}."
      cli_abort(msg, parent = cnd)
    }
  )
}

dbplyr_create_index <- function(con, ...) {
  dbplyr_fallback(con, "db_create_index", ...)
}
#' @export
#' @importFrom dplyr db_create_index
db_create_index.DBIConnection <- function(con,
                                          table,
                                          columns,
                                          name = NULL,
                                          unique = FALSE,
                                          ...) {
  sql <- sql_table_index(con, table, columns, name = name, unique = unique, ...)
  tryCatch(
    DBI::dbExecute(con, sql),
    error = function(cnd) {
      msg <- "Can't create index on {.val {table}}."
      cli_abort(msg, parent = cnd)
    }
  )
}

dbplyr_explain <- function(con, ...) {
  dbplyr_fallback(con, "db_explain", ...)
}
#' @export
#' @importFrom dplyr db_explain
db_explain.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_explain(con, sql, ...)
  call <- current_call()
  tryCatch(
    {
      expl <- DBI::dbGetQuery(con, sql)
    }, error = function(cnd) {
      cli_abort("Can't explain query.", parent = cnd)
    }
  )

  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

dbplyr_query_fields <- function(con, ...) {
  dbplyr_fallback(con, "db_query_fields", ...)
}
#' @export
#' @importFrom dplyr db_query_fields
db_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- sql_query_fields(con, sql, ...)
  tryCatch(
    {
      df <- DBI::dbGetQuery(con, sql)
    }, error = function(cnd) {
      cli_abort("Can't query fields.", parent = cnd)
    }
  )
  names(df)
}

dbplyr_save_query <- function(con, ...) {
  dbplyr_fallback(con, "db_save_query", ...)
}
#' @export
#' @importFrom dplyr db_save_query
db_save_query.DBIConnection <- function(con, sql, name, temporary = TRUE, ...) {
  sql <- sql_query_save(con, sql, name, temporary = temporary, ...)
  tryCatch(
    DBI::dbExecute(con, sql, immediate = TRUE),
    error = function(cnd) {
      cli_abort("Can't save query to {.val {name}}.", parent = cnd)
    }
  )
  name
}

dbplyr_sql_subquery <- function(con, ...) {
  dbplyr_fallback(con, "sql_subquery", ...)
}
#' @export
#' @importFrom dplyr sql_subquery
sql_subquery.DBIConnection <- function(con, from, name = unique_subquery_name(), ..., lvl = 0) {
  sql_query_wrap(con, from = from, name = name, ..., lvl = lvl)
}
