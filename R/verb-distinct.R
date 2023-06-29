#' Subset distinct/unique rows
#'
#' This is a method for the dplyr [distinct()] generic. It adds the
#' `DISTINCT` clause to the SQL query.
#'
#' @inheritParams arrange.tbl_lazy
#' @inheritParams dplyr::distinct
#' @inherit arrange.tbl_lazy return
#' @export
#' @importFrom dplyr distinct
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' db <- memdb_frame(x = c(1, 1, 2, 2), y = c(1, 2, 1, 1))
#' db %>% distinct() %>% show_query()
#' db %>% distinct(x) %>% show_query()
distinct.tbl_lazy <- function(.data, ..., .keep_all = FALSE) {
  grps <- syms(op_grps(.data))
  empty_dots <- dots_n(...) == 0
  can_use_distinct <- !.keep_all || (empty_dots && is_empty(grps))
  if (!can_use_distinct) {
    .data <- .data %>%
      group_by(..., .add = TRUE) %>%
      filter(row_number() == 1L) %>%
      group_by(!!!grps)

    return(.data)
  }

  if (empty_dots) {
    dots <- quos(!!!syms(colnames(.data)))
  } else {
    dots <- partial_eval_dots(.data, ..., .named = FALSE)
    dots <- quos(!!!dots)
  }
  prep <- distinct_prepare_compat(.data, dots, group_vars = group_vars(.data))
  out <- dplyr::select(prep$data, prep$keep)

  out$lazy_query <- add_distinct(out)
  out
}

# copied from dplyr with minor changes (names -> colnames)
# https://github.com/tidyverse/dplyr/blob/main/R/distinct.R
distinct_prepare_compat <- function(.data,
                             vars,
                             group_vars = character(),
                             .keep_all = FALSE,
                             caller_env = caller_env(2),
                             error_call = caller_env()
                             ) {
  stopifnot(is_quosures(vars), is.character(group_vars))

  # If no input, keep all variables
  if (length(vars) == 0) {
    return(list(
      data = .data,
      vars = seq_along(.data),
      keep = seq_along(.data)
    ))
  }

  # If any calls, use mutate to add new columns, then distinct on those
  computed_columns <- add_computed_columns(.data, vars, error_call = error_call)
  .data <- computed_columns$data
  distinct_vars <- computed_columns$added_names

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  missing_vars <- setdiff(distinct_vars, colnames(.data))
  if (length(missing_vars) > 0) {
    bullets <- c(
      "Must use existing variables.",
      set_names(glue("`{missing_vars}` not found in `.data`."), rep("x", length(missing_vars)))
    )
    abort(bullets, call = error_call)
  }

  # Only keep unique vars
  distinct_vars <- unique(distinct_vars)
  # Missing grouping variables are added to the front
  new_vars <- c(setdiff(group_vars, distinct_vars), distinct_vars)

  if (.keep_all) {
    keep <- seq_along(.data)
  } else {
    keep <- new_vars
  }

  list(data = .data, vars = new_vars, keep = keep)
}

# copied from dplyr
# https://github.com/tidyverse/dplyr/blob/main/R/group-by.R#L243
add_computed_columns <- function(.data,
                                 vars,
                                 error_call = caller_env()) {
  is_symbol <- purrr::map_lgl(vars, quo_is_variable_reference)
  needs_mutate <- have_name(vars) | !is_symbol

  if (any(needs_mutate)) {
    out <- mutate(.data, !!!vars)
    col_names <- names(exprs_auto_name(vars))
  } else {
    out <- .data
    col_names <- names(exprs_auto_name(vars))
  }

  list(data = out, added_names = col_names)
}

# copied from dplyr
# https://github.com/tidyverse/dplyr/blob/main/R/group-by.R#L276
quo_is_variable_reference <- function(quo) {
  if (quo_is_symbol(quo)) {
    return(TRUE)
  }

  if (quo_is_call(quo, n = 2)) {
    expr <- quo_get_expr(quo)

    if (is_call(expr, c("$", "[["))) {
      if (!identical(expr[[2]], sym(".data"))) {
        return(FALSE)
      }

      param <- expr[[3]]
      if (is_symbol(param) || is_string(param)) {
        return(TRUE)
      }
    }
  }

  FALSE
}


add_distinct <- function(.data) {
  lazy_query <- .data$lazy_query

  out <- lazy_select_query(
    x = lazy_query,
    distinct = TRUE
  )
  # TODO this could also work for joins
  if (!is_lazy_select_query(lazy_query)) {
    return(out)
  }

  # Optimisation overview
  # * `distinct()` adds the `DISTINCT` clause to `SELECT`
  # * `WHERE`, `GROUP BY`, and `HAVING` are executed before `SELECT`
  #   => they do not matter
  # * `ORDER BY`
  #   => but `arrange()` should not have an influence on `distinct()` so it
  #      should not matter
  # * `LIMIT` are executed after `SELECT`
  #   => needs a subquery
  if (!is_null(lazy_query$limit)) {
    return(out)
  }

  lazy_query$distinct <- TRUE
  lazy_query
}
