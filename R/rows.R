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

    sql <- sql_rows_update(x, y, by, returning_cols = returning_cols)
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
sql_rows_update <- function(x, y, by, ..., returning_cols = NULL) {
  ellipsis::check_dots_used()
  # FIXME: check here same src for x and y? if not -> error.
  UseMethod("sql_rows_update")
}

#' @export
sql_rows_update.tbl_lazy <- function(x, y, by, ..., returning_cols = NULL) {
  # avoid CTEs for the general case as they do not work everywhere
  p <- sql_rows_prep(x, y, by)

  sql <- paste0(
    "UPDATE ", p$name, "\n",
    "SET\n",
    paste0(
      "  ", unlist(p$new_columns_qq_list),
      " = ", unlist(p$new_columns_qual_qq_list),
      collapse = ",\n"
    ), "\n",
    "FROM (\n",
    "    ", sql_render(y), "\n",
    "  ) AS ", p$y_name, "\n",
    "WHERE (", p$compare_qual_qq, ")\n",
    sql_returning_cols(x, returning_cols)
  )

  glue::as_glue(sql)
}

#' @export
`sql_rows_update.tbl_Microsoft SQL Server` <- function(x, y, by, ..., returning_cols = NULL) {
  p <- sql_rows_prep(x, y, by)

  # https://stackoverflow.com/a/2334741/946850
  sql <- paste0(
    "WITH ", p$y_name, "(", p$y_columns_qq, ") AS (\n",
    sql_render(y),
    "\n)\n",
    #
    "UPDATE ", p$name, "\n",
    "SET\n",
    paste0(
      "  ", unlist(p$new_columns_qq_list),
      " = ", unlist(p$new_columns_qual_qq_list),
      collapse = ",\n"
    ),
    "\n",
    sql_output_cols(x, returning_cols),
    "FROM ", p$name, "\n",
    "  INNER JOIN ", p$y_name, "\n",
    "  ON ", p$compare_qual_qq
  )

  glue::as_glue(sql)
}

#' @export
sql_rows_update.tbl_MariaDBConnection <- function(x, y, by, ..., returning_cols = NULL) {
  p <- sql_rows_prep(x, y, by)

  # https://stackoverflow.com/a/19346375/946850
  sql <- paste0(
    "UPDATE ", p$name, "\n",
    "  INNER JOIN (\n", sql_render(y), "\n) AS ", p$y_name, "\n",
    "  ON ", p$compare_qual_qq, "\n",
    "SET\n",
    paste0("  ", p$new_columns_qual_qq, " = ", p$new_columns_qual_qq, collapse = ",\n"),
    sql_returning_cols(x, returning_cols)
  )

  glue::as_glue(sql)
}

db_key <- function (y, by) {
  if (is_null(by)) {
    set_names(1L, colnames(y)[[1]])
  } else {
    idx <- match(by, colnames(y))
    set_names(idx, by)
  }
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

sql_rows_prep <- function(x, y, by) {
  con <- remote_con(x)
  name <- remote_name(x)

  # https://stackoverflow.com/a/47753166/946850
  y_name <- escape(ident("...y"), con = con)
  y_q <- escape(colnames(y), con = con)
  by_q <- escape(by, con = con)
  # y_q <- DBI::dbQuoteIdentifier(con, colnames(y))
  # by_q <- DBI::dbQuoteIdentifier(con, by)

  y_columns_qq <- sql_list(y_q)
  y_columns_qual_qq <- sql_list(paste(y_name, ".", y_q))

  by_columns_qq <- sql_list(by_q)

  new_columns_q <- setdiff(y_q, by_q)
  new_columns_qual_q <- paste0(y_name, ".", new_columns_q)
  old_columns_qual_q <- paste0(name, ".", new_columns_q)

  new_columns_qq <- sql_list(new_columns_q)
  new_columns_qq_list <- list(new_columns_q)

  new_columns_qual_qq <- sql_list(new_columns_qual_q)
  new_columns_qual_qq_list <- list(new_columns_qual_q)

  new_columns_patch <- sql_coalesce(old_columns_qual_q, new_columns_qual_q)
  new_columns_patch_qq <- sql_list(new_columns_patch)
  new_columns_patch_qq_list <- list(new_columns_patch)

  compare_qual_qq <- paste0(
    y_name, ".", by_q,
    " = ",
    name, ".", by_q,
    collapse = " AND "
  )

  tibble(
    name, y_name,
    y_columns_qq,
    y_columns_qual_qq,
    by_columns_qq,
    new_columns_qq, new_columns_qq_list,
    new_columns_qual_qq, new_columns_qual_qq_list,
    new_columns_patch_qq, new_columns_patch_qq_list,
    compare_qual_qq
  )
}

sql_list <- function(x) {
  paste(x, collapse = ", ")
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
sql_returning_cols <- function(x, returning_cols, ...) {
  if (is_empty(returning_cols)) {
    return(NULL)
  }

  check_dots_empty()
  UseMethod("sql_returning_cols")
}

#' @export
sql_returning_cols.tbl_lazy <- function(x, returning_cols, ...) {
  con <- remote_con(x)
  returning_cols <- sql_named_cols(con, returning_cols, table = remote_name(x))

  paste0("RETURNING ", returning_cols)
}

#' @export
sql_returning_cols.tbl_duckdb_connection <- function(x, returning_cols, ...) {
  abort("DuckDB does not support the `returning` argument.")
}

#' @export
`sql_returning_cols.tbl_Microsoft SQL Server` <- function(x, returning_cols, ...) {
  NULL
}

sql_named_cols <- function(con, cols, table = NULL) {
  nms <- names2(cols)
  nms[nms == cols] <- ""

  cols <- DBI::dbQuoteIdentifier(con, cols)
  if (!is.null(table)) {
    cols <- paste0(table, ".", cols)
  }

  cols[nms != ""] <- paste0(cols, " AS ", DBI::dbQuoteIdentifier(con, nms[nms != ""]))
  paste0(cols, collapse = ", ")
}
