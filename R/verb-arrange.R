#' @export
arrange.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_single("arrange", .data, dots = dots)
}

#' @export
op_sort.op_arrange <- function(op) {
  c(op_sort(op$x), op$dots)
}

#' @export
op_desc.op_arrange <- function(x, ...) {
  op_desc(x$x, ...)
}

#' @export
sql_build.op_arrange <- function(op, con, ...) {
  order_vars <- translate_sql_(op$dots, con, context = list(clause = "ORDER"))
  group_vars <- c.sql(ident(op_grps(op$x)), con = con)

  select_query(sql_build(op$x, con), order_by = order_vars)
}
