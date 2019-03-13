#' @export
summarise.tbl_lazy <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  add_op_single("summarise", .data, dots = dots)
}

#' @export
op_vars.op_summarise <- function(op) {
  c(op_grps(op$x), names(op$dots))
}

#' @export
op_grps.op_summarise <- function(op) {
  grps <- op_grps(op$x)
  if (length(grps) == 1) {
    character()
  } else {
    grps[-length(grps)]
  }
}

#' @export
op_sort.op_summarise <- function(op) NULL

#' @export
sql_build.op_summarise <- function(op, con, ...) {
  select_vars <- translate_sql_(op$dots, con, window = FALSE, context = list(clause = "SELECT"))
  group_vars <- c.sql(ident(op_grps(op$x)), con = con)

  select_query(
    sql_build(op$x, con),
    select = c.sql(group_vars, select_vars, con = con),
    group_by = group_vars
  )
}
