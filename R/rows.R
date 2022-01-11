#' @export
#' @importFrom dplyr rows_update
#' @rdname rows-db
rows_update.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = NULL,
                                 check = NULL, returning = NULL) {
  by <- rows_check_key(by, x, y)
  y <- auto_copy(x, y, copy = copy)

  rows_check_key_df(x, by, df_name = "x")
  rows_check_key_df(y, by, df_name = "y")
  # TODO check that key values in `y` are unique? (argument `check`?)

  # Expect manual quote from user, silently fall back to enexpr()
  returning_expr <- enexpr(returning)
  tryCatch(
    returning_expr <- returning,
    error = identity
  )

  sim_data <- simulate_vars(x)
  returning_cols <- tidyselect::eval_select(returning_expr, sim_data) %>% names()

  new_columns <- setdiff(colnames(y), by)
  name <- target_table_name(x, in_place)

  if (!is_null(name)) {
    # TODO handle `returning_cols` here
    if (is_empty(new_columns)) {
      return(invisible(x))
    }

    sql <- sql_query_rows_update(
      con = remote_con(x),
      x_name = name,
      y = y,
      by = by,
      ...,
      returning_cols = returning_cols
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    existing_columns <- setdiff(colnames(x), new_columns)
    updated <- x %>%
      select(!!!existing_columns) %>%
      inner_join(y, by = by)

    if (is_empty(new_columns)) {
      out <- x
    } else {
      unchanged <- anti_join(x, y, by = by)
      out <- union_all(unchanged, updated)
    }

    if (!is_empty(returning_cols)) {
      if (inherits(updated, "tbl_TestConnection")) {
        abort("`returning` does not work for simulated connections")
      }

      returned_rows <- updated %>%
        select(!!!returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

#' @export
#' @rdname rows-db
sql_query_rows_update <- function(con, x_name, y, by, ..., returning_cols = NULL) {
  ellipsis::check_dots_used()
  # FIXME: check here same src for x and y? if not -> error.
  UseMethod("sql_query_rows_update")
}

#' @export
sql_query_rows_update.DBIConnection <- function(con, x_name, y, by, ...,
                                                returning_cols = NULL) {
  lvl <- 0

  parts <- update_prep(con, x_name, y, by, lvl)
  update_cols <- parts$update_cols
  update_values <- parts$update_values

  # avoid CTEs for the general case as they do not work everywhere
  clauses <- list(
    sql_clause_update(x_name),
    sql_clause_set(sql_escape_ident(con, update_cols), update_values),
    sql_clause_from(parts$from),
    sql_clause_where(parts$where),
    sql_returning_cols(con, returning_cols, x_name)
  )
  sql_format_clauses(clauses, lvl, con)
}

#' @export
`sql_query_rows_update.Microsoft SQL Server` <- function(con, x_name, y, by, ...,
                                                         returning_cols = NULL) {
  # https://stackoverflow.com/a/2334741/946850
  lvl <- 0

  parts <- update_prep(con, x_name, y, by, lvl)
  update_cols <- parts$update_cols
  update_values <- parts$update_values

  clauses <- list(
    sql_clause_update(ident(x_name)),
    sql_clause_set(sql_escape_ident(con, update_cols), update_values),
    sql_output_cols(con, returning_cols),
    sql_clause_from(ident(x_name)),
    sql_clause("INNER JOIN", parts$from),
    sql_clause("ON", parts$where, sep = " AND", lvl = 1)
  )
  sql_format_clauses(clauses, lvl, con)
}

#' @export
sql_query_rows_update.MariaDBConnection <- function(con, x_name, y, by, ...,
                                                    returning_cols = NULL) {
  # https://stackoverflow.com/a/19346375/946850
  lvl <- 0

  parts <- update_prep(con, x_name, y, by, lvl)
  update_cols <- parts$update_cols
  update_values <- parts$update_values

  clauses <- list(
    sql_clause_update(ident(x_name)),
    sql_clause("INNER JOIN", parts$from),
    sql_clause("ON", parts$where, sep = " AND", lvl = 1),
    sql_clause_set(sql_escape_ident(con, update_cols), update_values),
    sql_returning_cols(con, returning_cols, x_name)
  )
  sql_format_clauses(clauses, lvl, con)
}

update_prep <- function(con, x_name, y, by, lvl = 0) {
  y_name <- "...y"

  update_cols <- setdiff(colnames(y), by)
  update_values <- sql_table_prefix(con, update_cols, y_name)

  from <- dbplyr_sql_subquery(con,
    sql_render(y, con, subquery = TRUE, lvl = lvl + 1),
    name = y_name,
    lvl = lvl
  )

  join_by <- list(x = by, y = by, x_as = y_name, y_as = x_name)
  where <- sql_join_tbls(con, by = join_by, na_matches = "never")

  list(
    update_cols = update_cols,
    update_values = update_values,
    from = from,
    where = where
  )
}

sql_clause_update <- function(table) {
  sql_clause("UPDATE", table)
}

sql_clause_set <- function(lhs, rhs) {
  update_clauses <- sql(paste0(lhs, " = ", rhs))

  sql_clause("SET", update_clauses)
}

rows_check_key <- function(by, x, y, error_call = caller_env()) {
  if (is.null(by)) {
    by <- colnames(y)[[1]]
    msg <- glue("Matching, by = \"{by}\"")
    # TODO change or remove class?
    inform(msg, class = c("dplyr_message_matching_by", "dplyr_message"))
  }

  if (!is.character(by) || length(by) == 0) {
    abort("`by` must be a character vector.", call = error_call)
  }
  # is_named(by) checks all(names2(by) != ""), we need any(...)
  if (any(names2(by) != "")) {
    abort("`by` must be unnamed.", call = error_call)
  }

  bad <- setdiff(colnames(y), colnames(x))
  if (has_length(bad)) {
    abort("All columns in `y` must exist in `x`.", call = error_call)
  }

  by
}

rows_check_key_df <- function(df, by, df_name, error_call = caller_env()) {
  y_miss <- setdiff(by, colnames(df))
  if (length(y_miss) > 0) {
    msg <- glue("All `by` columns must exist in `{df_name}`.")
    abort(msg, call = error_call)
  }
}

target_table_name <- function (x, in_place) {
  name <- remote_name(x)
  if (!is_null(name) && is_true(in_place)) {
    return(name)
  }

  if (is_null(name) && is_true(in_place)) {
    abort("Can't determine name for target table. Set `in_place = FALSE` to return a lazy table.")
  }

  if (is_null(in_place)) {
    if (is_null(name)) {
      inform("Result is returned as lazy table, because `x` does not correspond to a table that can be updated. Use `in_place = FALSE` to mute this message.")
    }
    else {
      inform("Result is returned as lazy table. Use `in_place = FALSE` to mute this message, or `in_place = TRUE` to write to the underlying table.")
    }
  }

  NULL
}

rows_get_or_execute <- function(x, sql, returning_cols) {
  con <- remote_con(x)
  if (is_empty(returning_cols)) {
    dbExecute(con, sql, immediate = TRUE)
  } else {
    returned_rows <- dbGetQuery(con, sql, immediate = TRUE)
    x <- set_returned_rows(x, returned_rows)
  }

  invisible(x)
}

set_returned_rows <- function(x, returned_rows) {
  attr(x, "returned_rows") <- as_tibble(returned_rows)
  x
}

#' Extract and check the RETURNING rows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `get_returned_rows()` extracts the RETURNING rows produced by
#' [rows_insert()], [rows_update()], [rows_upsert()], or [rows_delete()]
#' if called with the `returning` argument.
#' An error is raised if this information is not available.
#'
#' @param x A lazy tbl.
#'
#' @return For `get_returned_rows()`, a tibble.
#'
#' @export
get_returned_rows <- function(x) {
  out <- attr(x, "returned_rows", TRUE)
  if (is.null(out)) {
    abort("No returned rows available.")
  }
  out
}

#' has_returned_rows()
#'
#' `has_returned_rows()` checks if `x` has stored RETURNING rows produced by
#' [rows_insert()], [rows_update()], [rows_upsert()], or [rows_delete()].
#'
#' @param x A lazy tbl.
#'
#' @return For `has_returned_rows()`, a scalar logical.
#'
#' @rdname get_returned_rows
#' @export
has_returned_rows <- function(x) {
  !identical(attr(x, "returned_rows"), NULL)
}

#' sql_returning_cols
#'
#' `sql_returning_cols()` and `sql_output_cols()` construct the SQL
#' required to support the `returning` argument.
#' Two methods are required, because the syntax for SQL Server
#' (and some other databases) is vastly different from Postgres and other
#' more standardized DBs.
#' @export
#' @rdname rows-db
sql_returning_cols <- function(con, cols, table, ...) {
  if (is_empty(cols)) {
    return(NULL)
  }

  ellipsis::check_dots_empty()
  UseMethod("sql_returning_cols")
}

#' @export
sql_returning_cols.DBIConnection <- function(con, cols, table, ...) {
  returning_cols <- sql_named_cols(con, cols, table = table)

  sql_clause("RETURNING", returning_cols)
}

#' @export
sql_returning_cols.duckdb_connection <- function(con, cols, ...) {
  abort("DuckDB does not support the `returning` argument.")
}

#' @export
`sql_returning_cols.Microsoft SQL Server` <- function(con, cols, ...) {
  NULL
}

#' @export
#' @param output_delete For `sql_output_cols()`, construct the SQL
#'   for a `DELETE` operation.
#' @rdname rows-db
sql_output_cols <- function(con, cols, output_delete = FALSE, ...) {
  if (is_empty(cols)) {
    return(NULL)
  }

  UseMethod("sql_output_cols")
}

#' @export
sql_output_cols.default <- function(con, cols, output_delete = FALSE, ...) {
  NULL
}

#' @export
`sql_output_cols.Microsoft SQL Server` <- function(con, cols, output_delete = FALSE, ...) {
  returning_cols <- sql_named_cols(
    con, cols,
    table = if (output_delete) "DELETED" else "INSERTED"
  )

  sql_clause("OUTPUT", returning_cols)
}

sql_named_cols <- function(con, cols, table = NULL) {
  nms <- names2(cols)
  nms[nms == cols] <- ""

  cols <- sql_table_prefix(con, cols, table)
  cols <- set_names(cols, nms)
  escape(ident_q(cols), collapse = NULL, con = con)
}
