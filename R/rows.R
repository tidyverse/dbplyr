#' Edit individual rows in the underlying database table
#'
#' @description
#' These are methods for the dplyr [rows_insert()], [`rows_append()`],
#' [`rows_update()`], [`rows_patch()`], [`rows_upsert()`], and [`rows_delete()`]
#' generics.
#'
#' When `in_place = TRUE` these verbs do not generate `SELECT` queries, but
#' instead directly modify the underlying data using `INSERT`, `UPDATE`, or
#' `DELETE` operators. This will require that you have write access to
#' the database: the connection needs permission to insert, modify or delete
#' rows, but not to alter the structure of the table.
#'
#' The default, `in_place = FALSE`, generates equivalent lazy tables (using
#' `SELECT` queries) that allow previewing the result without actually
#' modifying the underlying table on the database.
#'
#' @export
#' @param x A lazy table.
#'   For `in_place = TRUE`, this must be a table instantiated with [tbl()] or
#'   [compute()], not to a lazy query. The [remote_name()] function is used to
#'   determine the name of the table to be updated.
#' @param y A lazy table, data frame, or data frame extensions (e.g. a tibble).
#' @inheritParams dplyr::rows_insert
#' @param conflict For `rows_insert()`, how should keys in `y` that conflict
#'   with keys in `x` be handled? A conflict arises if there is a key in `y`
#'   that already exists in `x`.
#'
#'   One of:
#'   - `"error"`, the default, is not supported for database tables. To get the
#'     same behaviour add a unique index on the `by` columns and use
#'     `rows_append()`.
#'   - `"ignore"` will ignore rows in `y` with keys that conflict with keys in
#'     `x`.
#' @param unmatched For `rows_update()`, `rows_patch()`, and `rows_delete()`,
#'   how should keys in `y` that are unmatched by the keys in `x` be handled?
#'
#'   One of:
#'   - `"error"`, the default, is not supported for database tables. Add a
#'     foreign key constraint on the `by` columns of `y` to let the database
#'     check this behaviour for you.
#'   - `"ignore"` will ignore rows in `y` with keys that are unmatched by the
#'     keys in `x`.
#' @param in_place  Should `x` be modified in place? If `FALSE` will
#'   generate a `SELECT` query that returns the modified table; if `TRUE`
#'   will modify the underlying table using a DML operation (`INSERT`, `UPDATE`,
#'   `DELETE` or similar).
#' @param returning Columns to return. See [get_returned_rows()] for details.
#' @param method A string specifying the method to use. This is only relevant for
#'   `in_place = TRUE`.
#'
#' @importFrom dplyr rows_insert
#' @returns A new `tbl_lazy` of the modified data.
#'   With `in_place = FALSE`, the result is a lazy query that prints visibly,
#'   because the purpose of this operation is to preview the results.
#'   With `in_place = TRUE`, `x` is returned invisibly,
#'   because the purpose of this operation is the side effect of modifying rows
#'   in the table behind `x`.
#' @rdname rows-db
#' @examples
#' library(dplyr)
#'
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbExecute(con, "CREATE TABLE Ponies (
#'    id INTEGER PRIMARY KEY AUTOINCREMENT,
#'    name TEXT,
#'    cutie_mark TEXT
#' )")
#'
#' ponies <- tbl(con, "Ponies")
#'
#' applejack <- copy_inline(con, data.frame(
#'   name = "Apple Jack",
#'   cutie_mark = "three apples"
#' ))
#'
#' # The default behavior is to generate a SELECT query
#' rows_insert(ponies, applejack, conflict = "ignore")
#' # And the original table is left unchanged:
#' ponies
#'
#' # You can also choose to modify the table with in_place = TRUE:
#' rows_insert(ponies, applejack, conflict = "ignore", in_place = TRUE)
#' # In this case `rows_insert()` returns nothing and the underlying
#' # data is modified
#' ponies
rows_insert.tbl_lazy <- function(x,
                                 y,
                                 by = NULL,
                                 ...,
                                 conflict = c("error", "ignore"),
                                 copy = FALSE,
                                 in_place = FALSE,
                                 returning = NULL,
                                 method = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  conflict <- rows_check_conflict(conflict)

  y <- rows_auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y", unique = TRUE)

  new_columns <- setdiff(colnames(y), by)

  returning_cols <- rows_check_returning(x, returning, enexpr(returning))

  if (!is_null(name)) {
    sql <- sql_query_insert(
      con = remote_con(x),
      x_name = name,
      y = y,
      by = by,
      ...,
      conflict = conflict,
      returning_cols = returning_cols,
      method = method
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    out <- union_all(x, anti_join(y, x, by = by))

    if (!is_empty(returning_cols)) {
      # Need to `union_all()` with `x` so that all columns of `x` exist in the result
      returned_rows <- anti_join(y, x, by = by) %>%
        union_all(x %>% filter(0 == 1)) %>%
        select(!!!returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

#' @inheritParams dplyr::rows_append
#'
#' @export
#' @importFrom dplyr rows_append
#' @rdname rows-db
rows_append.tbl_lazy <- function(x,
                                 y,
                                 ...,
                                 copy = FALSE,
                                 in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- rows_auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  returning_cols <- rows_check_returning(x, returning, enexpr(returning))

  if (!is_null(name)) {
    sql <- sql_query_append(
      con = remote_con(x),
      x_name = name,
      y = y,
      ...,
      returning_cols = returning_cols
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    out <- union_all(x, y)

    if (!is_empty(returning_cols)) {
      # Need to `union_all()` with `x` so that all columns of `x` exist in the result
      returned_rows <- union_all(y, x %>% filter(0 == 1)) %>%
        select(!!!returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

#' @inheritParams dplyr::rows_update
#'
#' @export
#' @importFrom dplyr rows_update
#' @rdname rows-db
rows_update.tbl_lazy <- function(x,
                                 y,
                                 by = NULL,
                                 ...,
                                 unmatched = c("error", "ignore"),
                                 copy = FALSE,
                                 in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- rows_auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y", unique = TRUE)

  unmatched <- rows_check_ummatched(unmatched)

  new_columns <- setdiff(colnames(y), by)

  returning_cols <- rows_check_returning(x, returning, enexpr(returning))


  if (!is_null(name)) {
    # TODO handle `returning_cols` here
    if (is_empty(new_columns)) {
      return(invisible(x))
    }

    con <- remote_con(x)
    update_cols <- setdiff(colnames(y), by)
    update_values <- set_names(
      sql_table_prefix(con, update_cols, ident("...y")),
      update_cols
    )

    sql <- sql_query_update_from(
      con = con,
      x_name = name,
      y = y,
      by = by,
      update_values = update_values,
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
      returned_rows <- updated %>%
        select(!!!returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

#' @inheritParams dplyr::rows_patch
#'
#' @export
#' @importFrom dplyr rows_patch
#' @rdname rows-db
rows_patch.tbl_lazy <- function(x,
                                 y,
                                 by = NULL,
                                 ...,
                                 unmatched = c("error", "ignore"),
                                 copy = FALSE,
                                 in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- rows_auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y", unique = TRUE)

  unmatched <- rows_check_ummatched(unmatched)

  new_columns <- setdiff(colnames(y), by)

  returning_cols <- rows_check_returning(x, returning, enexpr(returning))

  if (!is_null(name)) {
    # TODO handle `returning_cols` here
    if (is_empty(new_columns)) {
      return(invisible(x))
    }

    con <- remote_con(x)

    update_cols <- setdiff(colnames(y), by)
    update_values <- sql_coalesce(
      sql_table_prefix(con, update_cols, name),
      sql_table_prefix(con, update_cols, ident("...y"))
    )
    update_values <- set_names(update_values, update_cols)

    sql <- sql_query_update_from(
      con = con,
      x_name = name,
      y = y,
      by = by,
      update_values = update_values,
      ...,
      returning_cols = returning_cols
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    to_patch <- inner_join(
      x, y,
      by = by,
      suffix = c("", "...y")
    )

    patch_columns_y <- paste0(new_columns, "...y")
    patch_quos <- lapply(new_columns, function(.x) quo(coalesce(!!sym(.x), !!sym(patch_columns_y)))) %>%
      rlang::set_names(new_columns)
    if (is_empty(new_columns)) {
      patched <- to_patch
      out <- x
    } else {
      patched <- to_patch %>%
        mutate(!!!patch_quos) %>%
        select(-all_of(patch_columns_y))
      unchanged <- anti_join(x, y, by = by)
      out <- union_all(unchanged, patched)
    }

    if (!is_empty(returning_cols)) {
      returned_rows <- patched %>%
        select(!!!returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

#' @export
#' @inheritParams dplyr::rows_upsert
#'
#' @importFrom dplyr rows_upsert
#' @rdname rows-db
rows_upsert.tbl_lazy <- function(x,
                                 y,
                                 by = NULL,
                                 ...,
                                 copy = FALSE,
                                 in_place = FALSE,
                                 returning = NULL,
                                 method = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- rows_auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y", unique = TRUE)

  returning_cols <- rows_check_returning(x, returning, enexpr(returning))

  new_columns <- setdiff(colnames(y), by)

  if (!is_null(name)) {
    # TODO use `rows_insert()` here
    if (is_empty(new_columns)) {
      return(invisible(x))
    }

    sql <- sql_query_upsert(
      con = remote_con(x),
      x_name = name,
      y = y,
      by = by,
      update_cols = setdiff(colnames(y), by),
      ...,
      returning_cols = returning_cols,
      method = method
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    inserted <- anti_join(y, x, by = by)

    if (is_empty(new_columns)) {
      unchanged <- x
      upserted <- inserted
    } else {
      unchanged <- anti_join(x, y, by = by)
      existing_columns <- setdiff(colnames(x), new_columns)
      updated <- x %>%
        select(!!!existing_columns) %>%
        inner_join(y, by = by)
      upserted <- union_all(updated, inserted)
    }

    out <- union_all(unchanged, upserted)

    if (!is_empty(returning_cols)) {
      returned_rows <- upserted %>%
        select(!!!returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

#' @export
#' @inheritParams dplyr::rows_delete
#'
#' @importFrom dplyr rows_delete
#' @rdname rows-db
rows_delete.tbl_lazy <- function(x,
                                 y,
                                 by = NULL,
                                 ...,
                                 unmatched = c("error", "ignore"),
                                 copy = FALSE,
                                 in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- rows_auto_copy(x, y, copy = copy)
  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y")

  unmatched <- rows_check_ummatched(unmatched)

  returning_cols <- rows_check_returning(x, returning, enexpr(returning))

  extra <- setdiff(colnames(y), by)
  if (!is_empty(extra)) {
    message <- glue("Ignoring extra `y` columns: ", commas(tick(extra)))
    inform(message, class = c("dplyr_message_delete_extra_cols", "dplyr_message"))
  }

  if (!is_null(name)) {
    sql <- sql_query_delete(
      con = remote_con(x),
      x_name = name,
      y = y,
      by = by,
      ...,
      returning_cols = returning_cols
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    out <- anti_join(x, y, by = by)

    if (!is_empty(returning_cols)) {
      returned_rows <- semi_join(x, y, by = by) %>%
        select(!!!returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

set_returned_rows <- function(x, returned_rows) {
  attr(x, "returned_rows") <- as_tibble(returned_rows)
  x
}

#' Extract and check the `RETURNING` rows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `get_returned_rows()` extracts the `RETURNING` rows produced by
#' [rows_insert()], [rows_append()], [rows_update()], [rows_upsert()],
#' or [rows_delete()] if these are called with the `returning` argument.
#' An error is raised if this information is not available.
#'
#' @param x A lazy tbl.
#'
#' @return For `get_returned_rows()`, a tibble.
#'
#' @export
#' @examples
#' library(dplyr)
#'
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbExecute(con, "CREATE TABLE Info (
#'    id INTEGER PRIMARY KEY AUTOINCREMENT,
#'    number INTEGER
#' )")
#' info <- tbl(con, "Info")
#'
#' rows1 <- copy_inline(con, data.frame(number = c(1, 5)))
#' rows_insert(info, rows1, conflict = "ignore", in_place = TRUE)
#' info
#'
#' # If the table has an auto incrementing primary key, you can use
#' # the returning argument + `get_returned_rows()` its value
#' rows2 <- copy_inline(con, data.frame(number = c(13, 27)))
#' info <- rows_insert(
#'   info,
#'   rows2,
#'   conflict = "ignore",
#'   in_place = TRUE,
#'   returning = id
#' )
#' info
#' get_returned_rows(info)
get_returned_rows <- function(x) {
  out <- attr(x, "returned_rows", TRUE)
  if (is.null(out)) {
    cli_abort("No returned rows available.")
  }
  out
}

#' has_returned_rows()
#'
#' `has_returned_rows()` checks if `x` has stored RETURNING rows produced by
#' [rows_insert()], [rows_append()], [rows_update()], [rows_upsert()],
#' or [rows_delete()].
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

#' @export
sql_returning_cols.duckdb_connection <- function(con, cols, ...) {
  cli_abort("DuckDB does not support the {.arg returning} argument.")
}

sql_coalesce <- function(x, y) {
  sql(paste0("COALESCE(", x, ", ", y, ")"))
}

# check helpers -----------------------------------------------------------

rows_check_by <- function(by, y, ..., error_call = caller_env()) {
  check_dots_empty()

  if (is.null(by)) {
    if (ncol(y) == 0L) {
      cli_abort("{.arg y} must have at least one column.", call = error_call)
    }

    by <- colnames(y)[[1]]

    inform(
      message = glue("Matching, by = \"{by}\""),
      class = c("dplyr_message_matching_by", "dplyr_message")
    )
  }

  if (!is.character(by)) {
    cli_abort("{.arg by} must be a character vector.", call = error_call)
  }
  if (is_empty(by)) {
    cli_abort("{.arg by} must specify at least 1 column.", call = error_call)
  }
  if (!all(names2(by) == "")) {
    cli_abort("{.arg by} must be unnamed.", call = error_call)
  }

  by
}

rows_check_containment <- function(x, y, ..., error_call = caller_env()) {
  check_dots_empty()

  bad <- setdiff(colnames(y), colnames(x))

  if (!is_empty(bad)) {
    bad <- err_vars(bad)

    message <- c(
      "All columns in {.arg y} must exist in {.arg x}.",
      i = "The following columns only exist in {.arg y}: {.field {bad}}."
    )

    cli_abort(message, call = error_call)
  }

  invisible()
}

rows_check_key <- function(x,
                           by,
                           arg,
                           ...,
                           unique = FALSE,
                           error_call = caller_env()) {
  check_dots_empty()

  missing <- setdiff(by, colnames(x))

  if (!is_empty(missing)) {
    missing <- err_vars(missing)

    message <- c(
      "All columns specified through {.arg by} must exist in {.arg x} and {.arg y}.",
      i = "The following columns are missing from {.arg {arg}}: {.field {missing}}."
    )

    cli_abort(message, call = error_call)
  }
}

rows_check_in_place <- function(df, in_place) {
  if (!rlang::is_bool(in_place)) {
    cli_abort("{.arg in_place} must be `TRUE` or `FALSE`.")
  }

  if (!in_place) return()

  if (inherits(df, "tbl_TestConnection")) {
    cli_abort("{.code in_place = TRUE} does not work for simulated connections.")
  }
}

rows_check_conflict <- function(conflict, error_call = caller_env()) {
  conflict <- arg_match(
    arg = conflict,
    values = c("error", "ignore"),
    error_arg = "conflict",
    error_call = error_call
  )

  if (conflict == "error") {
    cli_abort(
      c(
        '{.code conflict = "error"} is not supported for database tables.',
        i = 'Please use {.code conflict = "ignore"} instead'
      ),
      call = error_call
    )
  }

  conflict
}

rows_check_ummatched <- function(unmatched, error_call = caller_env()) {
  unmatched <- arg_match(
    arg = unmatched,
    values = c("error", "ignore"),
    error_arg = "ummatched",
    error_call = error_call
  )

  if (unmatched == "error") {
    cli_abort('{.code unmatched = "error"} is not supported for database tables.', call = error_call)
  }

  unmatched
}

rows_check_returning <- function(df, returning, returning_expr, error_call = caller_env()) {
  returning_cols <- eval_select2(returning_expr, df, error_call)

  if (is_empty(returning_cols)) return(returning_cols)

  if (inherits(df, "tbl_TestConnection")) {
    cli_abort("{.arg returning} does not work for simulated connections.", call = error_call)
  }

  returning_cols
}

err_vars <- function(x) {
  if (is.logical(x)) {
    x <- which(x)
  }
  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }

  glue::glue_collapse(x, sep = ", ", last = if (length(x) <= 2) " and " else ", and ")
}

commas <- function(...) paste0(..., collapse = ", ")

tick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}

# other helpers -----------------------------------------------------------

eval_select2 <- function(expr, data, error_call) {
  sim_data <- simulate_vars(data)
  locs <- fix_call(tidyselect::eval_select(expr, sim_data, error_call = error_call), error_call)
  names_out <- names(locs)
  set_names(colnames(sim_data)[locs], names_out)
}

target_table_name <- function(x, in_place) {
  # Never touch target table with `in_place = FALSE`
  if (!is_true(in_place)) {
    return(NULL)
  }

  name <- remote_name(x)
  if (is_null(name)) {
    cli_abort("Can't determine name for target table. Set {.code in_place = FALSE} to return a lazy table.")
  }

  name
}

rows_prep <- function(con, x_name, y, by, lvl = 0) {
  y_name <- ident("...y")
  from <- dbplyr_sql_subquery(con,
    sql_render(y, con, subquery = TRUE, lvl = lvl + 1),
    name = y_name,
    lvl = lvl
  )

  join_by <- list(x = by, y = by, x_as = y_name, y_as = x_name)
  where <- sql_join_tbls(con, by = join_by, na_matches = "never")

  list(
    from = from,
    where = where
  )
}

rows_insert_prep <- function(con, x_name, y, by, lvl = 0) {
  out <- rows_prep(con, x_name, y, by, lvl = lvl)

  join_by <- list(x = by, y = by, x_as = x_name, y_as = ident("...y"))
  where <- sql_join_tbls(con, by = join_by, na_matches = "never")
  out$conflict_clauses <- sql_clause_where_exists(x_name, where, not = TRUE)

  insert_cols <- escape(ident(colnames(y)), collapse = ", ", parens = TRUE, con = con)
  out$insert_clause <- sql_clause_insert(con, insert_cols, x_name)

  out
}

rows_auto_copy <- function(x, y, copy) {
  name <- remote_name(x)
  x_types <- get_col_types(remote_con(x), name)
  auto_copy(x, y, copy = copy, types = x_types)
}

get_col_types <- function(con, name) {
  if (inherits(con, "TestConnection")) {
    return(NULL)
  }
  if (is_null(name)) {
    return(NULL)
  }
  res <- DBI::dbSendQuery(con, paste0("SELECT * FROM ", name))
  on.exit(DBI::dbClearResult(res))
  DBI::dbFetch(res, n = 0)
  out <- DBI::dbColumnInfo(res)

  set_names(out[[".typname"]] %||% out[["type"]], out[["name"]])
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
