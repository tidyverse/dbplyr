#' @export
distinct.tbl_lazy <- function(.data, ..., .keep_all = FALSE) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval_dots(dots, vars = op_vars(.data))
  add_op_single("distinct", .data, dots = dots, args = list(.keep_all = .keep_all))
}

#' @export
op_vars.op_distinct <- function(op) {
  if (length(op$dots) == 0 || op$args$.keep_all) {
    op_vars(op$x)
  } else  {
    c(op_grps(op$x), names(op$dots))
  }
}

#' @export
sql_build.op_distinct <- function(op, con, ...) {
  if (length(op$dots) == 0) {
    select_query(
      sql_build(op$x, con),
      distinct = TRUE
    )
  } else {
    if (op$args$.keep_all) {
      stop(
        "Can't calculate distinct only on specified columns with SQL unless .keep_all is FALSE",
        call. = FALSE
      )
    }

    group_vars <- c.sql(ident(op_vars(op)), con = con)
    select_query(
      sql_build(op$x, con),
      select = group_vars,
      group_by = group_vars
    )
  }
}
