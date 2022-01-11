#' @export
#' @inheritParams dplyr::rows_update
#' @param returning Columns to return.
#'
#' @importFrom dplyr rows_update
#' @rdname rows-db
rows_update.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = NULL,
                                 returning = NULL) {
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

    con <- remote_con(x)
    update_cols <- setdiff(colnames(y), by)
    update_values <- set_names(
      sql_table_prefix(con, update_cols, "...y"),
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

#' @inheritParams dplyr::rows_patch
#'
#' @export
#' @importFrom dplyr rows_patch
#' @rdname rows-db
rows_patch.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = NULL,
                                returning = NULL) {
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

    con <- remote_con(x)
    update_cols <- setdiff(colnames(y), by)
    update_values <- sql_coalesce(
      sql_table_prefix(con, update_cols, name),
      sql_table_prefix(con, update_cols, "...y")
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
      if (inherits(patched, "tbl_TestConnection")) {
        abort("`returning` does not work for simulated connections")
      }

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
rows_upsert.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = NULL,
                                 returning = NULL) {
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

    con <- remote_con(x)
    update_cols <- setdiff(colnames(y), by)
    # update_values <- set_names(
    #   sql_table_prefix(con, update_cols, "...y"),
    #   update_cols
    # )

    sql <- sql_query_upsert(
      con = con,
      x_name = name,
      y = y,
      by = by,
      update_cols = update_cols,
      ...,
      returning_cols = returning_cols
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    existing_columns <- setdiff(colnames(x), new_columns)

    unchanged <- anti_join(x, y, by = by)
    inserted <- anti_join(y, x, by = by)
    updated <-
      x %>%
      select(!!!existing_columns) %>%
      inner_join(y, by = by)
    upserted <- union_all(updated, inserted)

    out <- union_all(unchanged, upserted)

    if (!is_empty(returning_cols)) {
      if (inherits(patched, "tbl_TestConnection")) {
        abort("`returning` does not work for simulated connections")
      }

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
rows_delete.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = NULL,
                                 returning = NULL) {
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

  returning_cols <- eval_select2(returning_expr, x)
  name <- target_table_name(x, in_place)

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
        select(returning_cols) %>%
        collect()
      out <- set_returned_rows(out, returned_rows)
    }

    out
  }
}

eval_select2 <- function(expr, data) {
  sim_data <- simulate_vars(data)
  locs <- tidyselect::eval_select(expr, sim_data)
  names_out <- names(locs)
  set_names(colnames(sim_data)[locs], names_out)
}

update_prep <- function(con, x_name, y, by, lvl = 0) {
  y_name <- "...y"
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

sql_coalesce <- function(x, y) {
  sql(paste0("COALESCE(", x, ", ", y, ")"))
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

#' @export
sql_returning_cols.duckdb_connection <- function(con, cols, ...) {
  abort("DuckDB does not support the `returning` argument.")
}

sql_named_cols <- function(con, cols, table = NULL) {
  nms <- names2(cols)
  nms[nms == cols] <- ""

  cols <- sql_table_prefix(con, cols, table)
  cols <- set_names(cols, nms)
  escape(ident_q(cols), collapse = NULL, con = con)
}
