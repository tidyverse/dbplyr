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
    stop("`.preserve` is not supported on database backends", call. = FALSE)
  }

  dots <- quos(...)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  .data$lazy_query <- add_filter(.data, dots)
  .data
}

add_filter <- function(.data, dots) {
  con <- remote_con(.data)
  lazy_query <- .data$lazy_query

  vars <- op_vars(.data)
  if (!uses_window_fun(dots, con)) {
    lazy_select_query(
      from = lazy_query,
      last_op = "filter",
      select = syms(set_names(vars)),
      where = dots
    )
  } else {
    # Do partial evaluation, then extract out window functions
    where <- translate_window_where_all(dots, ls(dbplyr_sql_translation(con)$window))

    # Convert where$expr back to a lazy dots object, and then
    # create mutate operation
    mutated <- mutate(.data, !!!where$comp)

    lazy_select_query(
      from = mutated$lazy_query,
      last_op = "filter",
      select = syms(set_names(vars)),
      where = where$expr
    )
  }
}
