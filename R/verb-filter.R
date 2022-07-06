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
#' db %>% filter(x < 5) %>% show_query()
#' db %>% filter(is.na(x)) %>% show_query()
# registered onLoad
#' @importFrom dplyr filter
filter.tbl_lazy <- function(.data, ..., .preserve = FALSE) {
  if (!identical(.preserve, FALSE)) {
    cli_abort("{.arg .preserve} is not supported on database backends")
  }
  check_filter(...)

  dots <- partial_eval_dots(.data, ..., .named = FALSE)

  if (is_empty(dots)) {
    return(.data)
  }

  .data$lazy_query <- add_filter(.data, dots)
  .data
}

add_filter <- function(.data, dots) {
  con <- remote_con(.data)
  lazy_query <- .data$lazy_query

  if (!uses_window_fun(dots, con)) {
    lazy_select_query(
      x = lazy_query,
      last_op = "filter",
      where = dots
    )
  } else {
    # Do partial evaluation, then extract out window functions
    where <- translate_window_where_all(dots, ls(dbplyr_sql_translation(con)$window))

    # Add extracted window expressions as columns
    mutated <- mutate(.data, !!!where$comp)

    # And filter with the modified `where` using the new columns
    original_vars <- op_vars(.data)
    lazy_select_query(
      x = mutated$lazy_query,
      last_op = "filter",
      select = syms(set_names(original_vars)),
      where = where$expr
    )
  }
}

check_filter <- function(...) {
  dots <- enquos(...)
  named <- have_name(dots)

  for (i in which(named)) {
    quo <- dots[[i]]

    # Unlike in `dplyr` named logical vectors do not make sense so they are
    # also not allowed
    expr <- quo_get_expr(quo)
    cli_abort(c(
      "Problem with {.fun filter} input `..{i}`.",
      x = "Input `..{i}` is named.",
      i = "This usually means that you've used {.code =} instead of {.code ==}.",
      i = "Did you mean `{names(dots)[i]} == {as_label(expr)}`?"
    ), call = caller_env())
  }
}
