# select ------------------------------------------------------------------

#' @export
select.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  add_op_single("select", .data, dots = dots)
}

#' @export
sql_build.op_select <- function(op, con, ...) {
  vars <- tidyselect::vars_select(op_vars(op$x), !!! op$dots, .include = op_grps(op$x))
  select_query(sql_build(op$x, con), ident(vars))
}

#' @export
op_vars.op_select <- function(op) {
  names(tidyselect::vars_select(op_vars(op$x), !!! op$dots, .include = op_grps(op$x)))
}


# rename ------------------------------------------------------------------

#' @export
rename.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("rename", .data, dots = dots)
}

#' @export
op_vars.op_rename <- function(op) {
  names(rename_vars(op_vars(op$x), !!! op$dots))
}

#' @export
sql_build.op_rename <- function(op, con, ...) {
  vars <- tidyselect::vars_rename(op_vars(op$x), !!! op$dots)
  select_query(sql_build(op$x, con), ident(vars))
}

