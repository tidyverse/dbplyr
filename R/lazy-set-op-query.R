#' @export
#' @rdname sql_build
lazy_set_op_query <- function(x, y, type = type, all = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      type = type,
      all = all
    ),
    class = c("lazy_set_op_query", "lazy_query")
  )
}

#' @export
print.lazy_set_op_query <- function(x, ..., con = NULL) {
  cat_line("<SQL ", toupper(x$type), ">")

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x, simulate_dbi())))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y, simulate_dbi())))
}

#' @export
op_vars.lazy_set_op_query <- function(op) {
  union(op_vars(op$x), op_vars(op$y))
}

#' @export
op_grps.lazy_set_op_query <- function(op) {
  op_grps(op$x)
}

#' @export
op_sort.lazy_set_op_query <- function(op) {
  op_sort(op$x)
}

#' @export
sql_build.lazy_set_op_query <- function(op, con, ...) {
  # add_op_set_op() ensures that both have same variables
  set_op_query(
    sql_optimise(sql_build(op$x, con), con),
    sql_optimise(sql_build(op$y, con), con),
    type = op$type,
    all = op$all
  )
}
