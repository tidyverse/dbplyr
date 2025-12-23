#' Subset rows using column values
#'
#' This is a method for the dplyr [filter()] generic. It generates the
#' `WHERE` clause of the SQL query.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::filter
#' @param .preserve Not supported by this method.
#' @inherit arrange.tbl_lazy return
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = c(2, NA, 5, NA, 10), y = 1:5)
#' db |> filter(x < 5) |> show_query()
#' db |> filter(is.na(x)) |> show_query()
#' @importFrom dplyr filter
# Registered onLoad
filter.tbl_lazy <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_unsupported_arg(.preserve, FALSE)
  check_filter(...)
  by <- compute_by(
    {{ .by }},
    .data,
    by_arg = ".by",
    data_arg = ".data",
    error_call = caller_env()
  )

  if (missing(...)) {
    return(.data)
  }

  if (by$from_by) {
    .data$lazy_query$group_vars <- by$names
  }
  dots <- partial_eval_dots(.data, ..., .named = FALSE)
  .data$lazy_query <- add_filter(.data, dots)
  if (by$from_by) {
    .data$lazy_query$group_vars <- character()
  }
  .data
}

add_filter <- function(.data, dots) {
  con <- remote_con(.data)
  lazy_query <- .data$lazy_query
  dots <- unname(dots)

  if (uses_window_fun(dots, con)) {
    # Do partial evaluation, then extract out window functions
    where <- translate_window_where_all(
      dots,
      env_names(dbplyr_sql_translation(con)$window)
    )

    # Add extracted window expressions as columns
    mutated <- mutate(.data, !!!where$comp)

    # And filter with the modified `where` using the new columns
    original_vars <- op_vars(.data)
    lazy_select_query(
      x = mutated$lazy_query,
      select = syms(set_names(original_vars)),
      where = where$expr
    )
  } else if (filter_can_use_having(lazy_query)) {
    filter_via_having(lazy_query, dots)
  } else if (filter_can_inline(dots, lazy_query, con)) {
    # WHERE processed before SELECT
    dots <- replace_sym(dots, lazy_query$select$name, lazy_query$select$expr)

    lazy_query$where <- c(lazy_query$where, dots)
    lazy_query
  } else {
    lazy_select_query(x = lazy_query, where = dots)
  }
}

filter_can_inline <- function(dots, lazy_query, con) {
  if (!is_lazy_select_query(lazy_query)) {
    return(FALSE)
  }

  if (uses_mutated_vars(dots, lazy_query$select)) {
    return(FALSE)
  }

  if (uses_window_fun(lazy_query$select$expr, con)) {
    return(FALSE)
  }

  if (any_expr_uses_sql(lazy_query$select$expr)) {
    return(FALSE)
  }

  TRUE
}

filter_can_use_having <- function(lazy_query) {
  # From the Postgres documentation: https://www.postgresql.org/docs/current/sql-select.html#SQL-HAVING
  # Each column referenced in condition must unambiguously reference a grouping
  # column, unless the reference appears within an aggregate function or the
  # ungrouped column is functionally dependent on the grouping columns.

  # After `summarise()` every column is either
  # * a grouping column
  # * or an aggregated column
  # (this is not the case for data frames but valid for SQL tables)
  #
  # Therefore, if `filter()` does not use a window function, then we only use
  # grouping or aggregated columns
  if (!is_lazy_select_query(lazy_query)) {
    FALSE
  } else {
    lazy_query$select_operation == "summarise"
  }
}

filter_via_having <- function(lazy_query, dots) {
  # ANSI SQL processes HAVING before SELECT
  dots <- replace_sym(dots, lazy_query$select$name, lazy_query$select$expr)

  lazy_query$having <- c(lazy_query$having, dots)
  lazy_query
}

check_filter <- function(...) {
  dots <- enquos(...)
  named <- have_name(dots)

  for (i in which(named)) {
    quo <- dots[[i]]

    # Unlike in `dplyr` named logical vectors do not make sense so they are
    # also not allowed
    expr <- quo_get_expr(quo)
    cli_abort(
      c(
        "Problem with {.fun filter} input `..{i}`.",
        x = "Input `..{i}` is named.",
        i = "This usually means that you've used {.code =} instead of {.code ==}.",
        i = "Did you mean `{names(dots)[i]} == {as_label(expr)}`?"
      ),
      call = caller_env()
    )
  }
}
