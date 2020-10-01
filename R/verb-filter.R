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
  add_op_single("filter", .data, dots = dots)
}

#' @export
sql_build.op_filter <- function(op, con, ...) {
  vars <- op_vars(op$x)

  if (!uses_window_fun(op$dots, con)) {
    where_sql <- translate_sql_(op$dots, con, context = list(clause = "WHERE"))

    select_query(
      sql_build(op$x, con),
      where = where_sql
    )
  } else {
    # Do partial evaluation, then extract out window functions
    where <- translate_window_where_all(op$dots, ls(dbplyr_sql_translation(con)$window))

    # Convert where$expr back to a lazy dots object, and then
    # create mutate operation
    mutated <- sql_build(new_op_select(op$x, carry_over(vars, where$comp)), con = con)
    where_sql <- translate_sql_(where$expr, con = con, context = list(clause = "WHERE"))

    select_query(mutated, select = ident(vars), where = where_sql)
  }
}
