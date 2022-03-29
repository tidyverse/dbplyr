#' @export
#' @inheritParams dplyr::rows_insert
#' @param returning Columns to return.
#'
#' @importFrom dplyr rows_insert
#' @rdname rows-db
rows_insert.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- auto_copy(x, y, copy = copy)

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
      returning_cols = returning_cols
    )

    rows_get_or_execute(x, sql, returning_cols)
  } else {
    out <- union_all(x, anti_join(y, x, by = by))

    if (!is_empty(returning_cols)) {
      # Need to `union_all()` with `x` so that all columns of `x` exist in the result
      returned_rows <- anti_join(y, x, by = by) %>%
        union_all(x %>% filter(0 == 1)) %>%
        select(returning_cols) %>%
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
rows_update.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y", unique = TRUE)

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
rows_patch.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE,
                                returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y", unique = TRUE)

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
rows_upsert.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- auto_copy(x, y, copy = copy)

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
      returning_cols = returning_cols
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
rows_delete.tbl_lazy <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE,
                                 returning = NULL) {
  check_dots_empty()
  rows_check_in_place(x, in_place)
  name <- target_table_name(x, in_place)

  y <- auto_copy(x, y, copy = copy)

  rows_check_containment(x, y)

  by <- rows_check_by(by, y)

  rows_check_key(x, by, "x")
  rows_check_key(y, by, "y")

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

sql_coalesce <- function(x, y) {
  sql(paste0("COALESCE(", x, ", ", y, ")"))
}

# check helpers -----------------------------------------------------------

rows_check_by <- function(by, y, ..., error_call = caller_env()) {
  check_dots_empty()

  if (is.null(by)) {
    if (ncol(y) == 0L) {
      abort("`y` must have at least one column.", call = error_call)
    }

    by <- colnames(y)[[1]]

    inform(
      message = glue("Matching, by = \"{by}\""),
      class = c("dplyr_message_matching_by", "dplyr_message")
    )
  }

  if (!is.character(by)) {
    abort("`by` must be a character vector.", call = error_call)
  }
  if (is_empty(by)) {
    abort("`by` must specify at least 1 column.", call = error_call)
  }
  if (!all(names2(by) == "")) {
    abort("`by` must be unnamed.", call = error_call)
  }

  by
}

rows_check_containment <- function(x, y, ..., error_call = caller_env()) {
  check_dots_empty()

  bad <- setdiff(colnames(y), colnames(x))

  if (!is_empty(bad)) {
    bad <- err_vars(bad)

    message <- c(
      "All columns in `y` must exist in `x`.",
      i = glue("The following columns only exist in `y`: {bad}.")
    )

    abort(message, call = error_call)
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
      "All columns specified through `by` must exist in `x` and `y`.",
      i = glue("The following columns are missing from `{arg}`: {missing}.")
    )

    abort(message, call = error_call)
  }

  # TODO adapt this
  # out <- x[by]
  #
  # if (unique && vec_duplicate_any(out)) {
  #   duplicated <- vec_duplicate_detect(out)
  #   duplicated <- which(duplicated)
  #   duplicated <- err_locs(duplicated)
  #
  #   message <- c(
  #     glue("`{arg}` key values must be unique."),
  #     i = glue("The following rows contain duplicate key values: {duplicated}.")
  #   )
  #
  #   abort(message, call = error_call)
  # }
  #
  # out
}

# rows_check_conflict <- function(conflict, ..., error_call = caller_env()) {
#   check_dots_empty()
#
#   arg_match(
#     arg = conflict,
#     values = c("error", "ignore"),
#     error_arg = "conflict",
#     error_call = error_call
#   )
# }

rows_check_in_place <- function(df, in_place) {
  if (!rlang::is_bool(in_place)) {
    abort("`in_place` must be `TRUE` or `FALSE`.")
  }

  if (!in_place) return()

  if (inherits(df, "tbl_TestConnection")) {
    abort("`in_place = TRUE` does not work for simulated connections.")
  }
}

rows_check_returning <- function(df, returning, returning_expr) {
  # Expect manual quote from user, silently fall back to enexpr()
  tryCatch(
    returning_expr <- returning,
    error = identity
  )

  returning_cols <- eval_select2(returning_expr, df)

  if (is_empty(returning_cols)) return(returning_cols)

  if (inherits(df, "tbl_TestConnection")) {
    abort("`returning` does not work for simulated connections.")
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

eval_select2 <- function(expr, data) {
  sim_data <- simulate_vars(data)
  locs <- tidyselect::eval_select(expr, sim_data)
  names_out <- names(locs)
  set_names(colnames(sim_data)[locs], names_out)
}

target_table_name <- function(x, in_place) {
  name <- remote_name(x)
  if (!is_null(name) && is_true(in_place)) {
    return(name)
  }

  if (is_null(name) && is_true(in_place)) {
    abort("Can't determine name for target table. Set `in_place = FALSE` to return a lazy table.")
  }

  NULL
}

rows_prep <- function(con, x_name, y, by, lvl = 0) {
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
