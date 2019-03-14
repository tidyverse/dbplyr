#' @export
distinct.tbl_lazy <- function(.data, ..., .keep_all = FALSE) {
  if (dots_n(...) > 0) {
    if (.keep_all) {
      stop(
        "Can only find distinct value of specified columns if .keep_all is FALSE",
        call. = FALSE
      )
    }

    .data <- transmute(.data, ...)
  }

  add_op_single("distinct", .data, dots = list())
}

#' @export
op_vars.op_distinct <- function(op) {
  c(op_grps(op$x), op_vars(op$x))
}

#' @export
sql_build.op_distinct <- function(op, con, ...) {
  select_query(
    sql_build(op$x, con),
    distinct = TRUE
  )
}
