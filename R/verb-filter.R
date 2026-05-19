#' Keep or drop rows that match a condition
#'
#' @description
#' These are methods for the dplyr [dplyr::filter()] and [dplyr::filter_out()]
#' generics. They generate the `WHERE` clause of the SQL query.
#'
#' `filter()` is translated directly to `WHERE`, which already matches dplyr's
#' behaviour of treating `NA` like `FALSE` (SQL's three-valued logic drops
#' `NULL` rows from `WHERE`).
#'
#' `filter_out()` requires an additional step, where the combined condition
#' is wrapped in `is_distinct_from(., TRUE)`, which is then translated using
#' the backend (e.g. to `IS DISTINCT FROM` on PostgreSQL, `IS NOT` on SQLite).
#' This ensures that the SQL translation matches dplyr's semantics.
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
#' db |> filter_out(x < 5) |> show_query()
#' db |> filter(is.na(x)) |> show_query()
#' @importFrom dplyr filter
#' @exportS3Method NULL
# Registered onLoad
filter.tbl_lazy <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_unsupported_arg(.preserve, FALSE)
  check_filter(..., .fn = "filter")
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

  .data <- set_by_groups(.data, by)
  dots <- partial_eval_dots(.data, ..., .named = FALSE)
  .data$lazy_query <- add_filter(.data$lazy_query, remote_con(.data), dots)
  clear_by_groups(.data, by)
}

#' @rdname filter.tbl_lazy
#' @importFrom dplyr filter_out
#' @export
filter_out.tbl_lazy <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_unsupported_arg(.preserve, FALSE)
  check_filter(..., .fn = "filter_out")
  by <- compute_by(
    {{ .by }},
    .data,
    by_arg = ".by",
    data_arg = ".data",
    error_call = caller_env()
  )

  if (missing(...)) {
    return(filter(.data, FALSE))
  }

  .data <- set_by_groups(.data, by)

  # Multiple conditions are AND-combined to match dplyr's semantics, then
  # wrapped in `is_distinct_from(., TRUE)`. The backend translation of
  # `is_distinct_from()` provides the dialect-specific SQL.
  dots <- partial_eval_dots(.data, ..., .named = FALSE)
  dots_and <- Reduce(function(a, b) expr((!!a) & (!!b)), dots)
  dots2 <- list(expr(is_distinct_from(!!dots_and, TRUE)))

  .data$lazy_query <- add_filter(.data$lazy_query, remote_con(.data), dots2)

  clear_by_groups(.data, by)
}

add_filter <- function(lazy_query, con, exprs) {
  exprs <- unname(exprs)

  if (uses_window_fun(exprs, con)) {
    # Extract out window functions into new mutate layer
    where <- translate_window_where_all(exprs, window_funs(con))

    # add_mutate() always creates a subquery, so we need to bring all
    # existing variables along for the ride
    original_vars <- op_vars(lazy_query)
    new_exprs <- c(syms(set_names(original_vars)), where$comp)
    mutated <- add_mutate(lazy_query, new_exprs)

    # filter with the modified `where` using the new columns
    lazy_select_query(
      x = mutated,
      select = syms(set_names(original_vars)),
      where = where$expr
    )
  } else if (filter_can_use_having(lazy_query)) {
    filter_via_having(lazy_query, exprs)
  } else if (inherits(lazy_query, "lazy_multi_join_query")) {
    # Due to way JOINs generate their SELECT clauses much later we have to
    # do the backtransformation of variable names in `sql_build.lazy_multi_join_query`
    lazy_query$where <- c(lazy_query$where, exprs)
    lazy_query
  } else if (can_inline_filter(exprs, lazy_query, con)) {
    # WHERE processed before SELECT
    exprs <- replace_sym(exprs, lazy_query$select$name, lazy_query$select$expr)

    lazy_query$where <- c(lazy_query$where, exprs)
    lazy_query
  } else {
    lazy_select_query(x = lazy_query, where = exprs)
  }
}

# filter() adds the WHERE clause
# * WHERE is executed before SELECT
#   => can't reference columns computed in SELECT
#   => can't inline if SELECT uses window functions (evaluated after WHERE)
#   => can't inline if SELECT uses sql() (can't extract column references)
can_inline_filter <- function(exprs, lazy_query, con) {
  if (!is_lazy_select_query(lazy_query)) {
    return(FALSE)
  }

  if (uses_mutated_vars(exprs, lazy_query$select)) {
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

filter_via_having <- function(lazy_query, exprs) {
  # ANSI SQL processes HAVING before SELECT
  exprs <- replace_sym(exprs, lazy_query$select$name, lazy_query$select$expr)

  lazy_query$having <- c(lazy_query$having, exprs)
  lazy_query
}

check_filter <- function(..., .fn) {
  dots <- enquos(...)
  named <- have_name(dots)

  for (i in which(named)) {
    quo <- dots[[i]]

    # Unlike in `dplyr` named logical vectors do not make sense so they are
    # also not allowed
    expr <- quo_get_expr(quo)
    cli_abort(
      c(
        "Problem with {.fun {(.fn)}} input `..{i}`.",
        x = "Input `..{i}` is named.",
        i = "This usually means that you've used {.code =} instead of {.code ==}.",
        i = "Did you mean `{names(dots)[i]} == {as_label(expr)}`?"
      ),
      call = caller_env()
    )
  }
}
