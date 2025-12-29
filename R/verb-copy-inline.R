#' Use a local data frame in a dbplyr query
#'
#' This is an alternative to [copy_to()] that does not need write access and
#' is faster for small data.
#'
#' It writes the data directly in the SQL query via the `VALUES` clause.
#'
#' @seealso [copy_to()] to copy the data into a new database table.
#' @export
#' @param con A database connection.
#' @param df A local data frame. The data is written directly in the SQL query
#'   so it should be small.
#' @param types A named character vector of SQL data types to use for the columns.
#'    The data types are backend specific. For example for Postgres this could
#'    be `c(id = "bigint", created_at = "timestamp", values = "integer[]")`.
#'    If `NULL`, the default, the types are determined from `df`.
#' @return A `tbl_lazy`.
#'
#' @examples
#' df <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' copy_inline(con, df)
#'
#' copy_inline(con, df) |> dplyr::show_query()
copy_inline <- function(con, df, types = NULL) {
  if (!inherits(df, "data.frame")) {
    cli_abort("{.var df} needs to be a data.frame.")
  }

  if (ncol(df) == 0) {
    cli_abort("{.var df} needs at least one column.")
  }

  if (!is_null(types)) {
    check_character(types)

    if (!setequal(colnames(df), names(types))) {
      cli_abort("Names of {.arg df} and {.arg types} must be the same.")
    }
  }

  lazy_query <- lazy_values_query(df, types)
  new_tbl_lazy(con, lazy_query)
}

lazy_values_query <- function(df, types) {
  lazy_base_query(
    class = "values",
    x = df,
    vars = colnames(df),
    col_types = types
  )
}

#' @export
sql_build.lazy_base_values_query <- function(op, con, ..., sql_options = NULL) {
  class(op) <- c("values_query", "query")
  op
}

#' @export
sql_render.values_query <- function(
  query,
  con = query$src$con,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  sql_values_subquery(con, query$x, types = query$col_types, lvl = lvl)
}

#' @export
flatten_query.values_query <- function(qry, query_list, con) {
  querylist_reuse_query(qry, query_list, con)
}

#' @export
op_vars.lazy_base_values_query <- function(op) {
  colnames(op$x)
}

sql_values_subquery <- function(con, df, types, lvl = 0, ...) {
  check_dots_used()
  dialect <- sql_dialect(con)
  return(sql_values_subquery_(dialect, df, types, lvl, ...))

  UseMethod("sql_values_subquery")
}
sql_values_subquery_ <- function(dialect, df, types, lvl = 0, ...) {
  UseMethod("sql_values_subquery")
}

#' @export
sql_values_subquery.DBIConnection <- function(con, df, types, lvl = 0, ...) {
  sql_values_subquery_default(con, df, types = types, lvl = lvl, row = FALSE)
}

#' @export
sql_values_subquery.sql_dialect <- sql_values_subquery.DBIConnection

sql_values_subquery_default <- function(con, df, types, lvl, row) {
  df <- values_prepare(con, df)
  if (nrow(df) == 0L) {
    return(sql_values_zero_rows(con, df, types, lvl))
  }

  # The query consists of two parts:
  # 1) An outer select which converts the values to the correct types. This needs
  # to use the translation of `as.<column type>(<column name>)` (e.g. `as.numeric(mpg)`)
  # because some backends need a special translation for some types e.g. casting
  # to logical/bool in MySQL
  #   `IF(<column name>, TRUE, FALSE)`
  # This is done with the help of `sql_cast_dispatch()` via dispatch on the
  # column type. The explicit cast is required so that joins work e.g. on date
  # columns in Postgres.
  # 2) A subquery which is the union of:
  #   a) a zero row table which is just required to name the columns. This is
  #      necessary as e.g. SQLite cannot name `VALUES`.
  #   b) `VALUES` clause
  cols_clause <- names_to_as(con, rep_named(colnames(df), "NULL"))

  null_row_clauses <- list(
    select = sql_clause_select(cols_clause),
    where = sql_clause_where(sql("0 = 1"))
  )

  rows_clauses <- sql_values_clause(con, df, row = row)
  rows_query <- sql_format_clauses(rows_clauses, lvl = lvl + 1)

  subquery <- sql_query_union(
    con,
    x = sql_format_clauses(null_row_clauses, lvl + 1),
    unions = list(table = as.character(rows_query), all = TRUE),
    lvl = lvl + 1
  )

  sql_query_select(
    con,
    select = sql_values_cast_clauses(con, df, types, na = FALSE),
    from = sql_query_wrap(con, subquery, name = "values_table", lvl = lvl),
    lvl = lvl
  )
}

sql_values_subquery_column_alias <- function(con, df, types, lvl, ...) {
  df <- values_prepare(con, df)
  if (nrow(df) == 0L) {
    return(sql_values_zero_rows(con, df, types, lvl))
  }

  # The `SELECT` clause converts the values to the correct types. This needs
  # to use the translation of `as.<column type>(<column name>)` (e.g. `as.numeric(mpg)`)
  # because some backends need a special translation for some types e.g. casting
  # to logical/bool in MySQL
  #   `IF(<column name>, TRUE, FALSE)`
  # This is done with the help of `sql_cast_dispatch()` via dispatch on the
  # column type. The explicit cast is required so that joins work e.g. on date
  # columns in Postgres.
  # The `FROM` clause is simply the `VALUES` clause with table and column alias
  rows_clauses <- sql_values_clause(con, df, row = FALSE)
  rows_query <- sql_format_clauses(rows_clauses, lvl = lvl + 1)

  table_alias_sql <- sql_glue2(con, "drvd({.id colnames(df)})")

  if (grepl("\\n", rows_query)) {
    rows_query <- sql(paste0(
      "(\n",
      rows_query,
      "\n",
      indent_lvl(") AS ", lvl),
      table_alias_sql
    ))
  } else {
    # indent is not perfect but okay
    rows_query <- sql(paste0("(", rows_query, ") AS ", table_alias_sql))
  }

  sql_query_select(
    con,
    select = sql_values_cast_clauses(con, df, types, na = FALSE),
    from = rows_query,
    lvl = lvl
  )
}

sql_values_subquery_union <- function(con, df, types, lvl, row, from = NULL) {
  df <- values_prepare(con, df)
  if (nrow(df) == 0L) {
    return(sql_values_zero_rows(con, df, types, lvl, from))
  }

  # The query consists of two parts:
  # 1) An outer select which converts the values to the correct types. This needs
  # to use the translation of `as.<column type>(<column name>)` (e.g. `as.numeric(mpg)`)
  # because some backends need a special translation for some types e.g. casting
  # to logical/bool in MySQL
  #   `IF(<column name>, TRUE, FALSE)`
  # This is done with the help of `sql_cast_dispatch()` via dispatch on the
  # column type. The explicit cast is required so that joins work e.g. on date
  # columns in Postgres.
  # 2) A subquery which is the union of:
  #   a) a zero row table which is just required to name the columns. This is
  #      necessary as e.g. SQLite cannot name `VALUES`.
  #   b) `UNION ALL` of one row `SELECT` statements
  cols_clause <- names_to_as(con, rep_named(colnames(df), "NULL"))

  clauses <- list(
    select = sql_clause_select(cols_clause),
    from = if (!is.null(from)) sql_clause_from(sql_escape_ident(con, from)),
    where = sql_clause_where(sql("0 = 1"))
  )
  null_row_query <- sql_format_clauses(clauses, lvl + 1)

  escaped_values <- purrr::map(
    df,
    escape,
    con = con,
    collapse = NULL,
    parens = FALSE
  )

  rows <- rlang::exec(paste, !!!escaped_values, sep = ", ")
  select_kw <- style_kw("SELECT ")
  tables <- paste0(lvl_indent(lvl + 1), select_kw, rows)
  if (!is_null(from)) {
    from_kw <- style_kw("FROM ")
    tables <- paste0(tables, " ", from_kw, from)
  }

  subquery <- sql_query_union(
    con,
    x = null_row_query,
    unions = list(all = TRUE, table = tables),
    lvl = lvl + 1
  )

  sql_query_select(
    con,
    select = sql_values_cast_clauses(con, df, types, na = FALSE),
    from = sql_query_wrap(con, subquery, name = "values_table", lvl = lvl),
    lvl = lvl
  )
}

sql_values_clause <- function(con, df, row = FALSE) {
  escaped_values <- purrr::map(
    df,
    escape,
    con = con,
    collapse = NULL,
    parens = FALSE
  )
  rows <- rlang::exec(paste, !!!escaped_values, sep = ", ")
  rows_sql <- sql(paste0(if (row) "ROW", "(", rows, ")"))

  list(sql_clause("VALUES", rows_sql))
}

sql_values_zero_rows <- function(con, df, types, lvl, from = NULL) {
  if (nrow(df) != 0L) {
    cli_abort("{.arg df} does not have 0 rows", .internal = TRUE)
  }

  typed_cols <- sql_values_cast_clauses(con, df, types, na = TRUE)

  clauses <- list(
    select = sql_clause_select(typed_cols),
    from = if (!is.null(from)) sql_clause_from(sql_escape_ident(con, from)),
    where = sql_clause_where(sql("0 = 1"))
  )
  sql_format_clauses(clauses, lvl)
}

sql_values_cast_clauses <- function(con, df, types, na) {
  if (is_null(types)) {
    typed_cols <- purrr::map2_chr(
      df,
      colnames(df),
      ~ {
        val <- if (na) NA else ident(.y)
        cast_expr <- call2(sql_cast_dispatch(.x), val)
        translate_sql(!!cast_expr, con = con)
      }
    )
  } else {
    typed_cols <- purrr::imap_chr(types, function(type, val) {
      val <- if (na) NA else ident(val)
      sql_glue2(con, "CAST({val} AS {.sql type})")
    })
  }

  names_to_as(con, typed_cols)
}

values_prepare <- function(con, df) {
  dialect <- sql_dialect(con)
  return(values_prepare_(dialect, df))

  UseMethod("values_prepare")
}
values_prepare_ <- function(dialect, df) {
  UseMethod("values_prepare")
}

#' @export
values_prepare.DBIConnection <- function(con, df) {
  df
}

#' @export
values_prepare.sql_dialect <- values_prepare.DBIConnection

# This
sql_cast_dispatch <- function(x) {
  UseMethod("sql_cast_dispatch")
}

#' @export
sql_cast_dispatch.sql <- function(x) {
  expr(as.character)
}

#' @export
sql_cast_dispatch.logical <- function(x) {
  expr(as.logical)
}

#' @export
sql_cast_dispatch.integer <- function(x) {
  expr(as.integer)
}

#' @export
sql_cast_dispatch.numeric <- function(x) {
  expr(as.numeric)
}

#' @export
sql_cast_dispatch.character <- function(x) {
  expr(as.character)
}

#' @export
sql_cast_dispatch.factor <- function(x) {
  expr(as.character)
}

#' @export
sql_cast_dispatch.Date <- function(x) {
  expr(as.Date)
}

#' @export
sql_cast_dispatch.POSIXct <- function(x) {
  expr(as.POSIXct)
}

#' @export
sql_cast_dispatch.integer64 <- function(x) {
  expr(as.integer64)
}

#' @export
sql_cast_dispatch.blob <- function(x) {
  expr(as.blob)
}

#' @importFrom dplyr coalesce
NULL
utils::globalVariables(c("as.integer64", "as.blob"))
