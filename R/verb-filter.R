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
  dots <- unname(dots)

  if (!uses_window_fun(dots, con)) {
    # TODO check for window_order?
    # TODO need to check if only grouping columns or aggregated columns?
    # -> probably not because after a summarise every column should be one of these options
    if (is_null(op_frame(lazy_query)) &&
        inherits(lazy_query, "lazy_select_query") &&
        lazy_query$select_operation == "summarise") {
      lazy_query$group_by <- syms(op_grps(lazy_query))

      names <- lazy_query$select$name
      exprs <- lazy_query$select$expr
      dots <- purrr::map(dots, replace_sym, names, exprs)

      lazy_query$having <- c(lazy_query$having, dots)
      return(lazy_query)
    }

    lazy_select_query(
      from = lazy_query,
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
      from = mutated$lazy_query,
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
