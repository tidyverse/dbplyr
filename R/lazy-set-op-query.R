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
print.lazy_set_op_query <- function(x, ...) {
  cat_line("<SQL ", toupper(x$type), ">")

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x)))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y)))
}

#' @export
sql_render.lazy_set_op_query <- function(query, con = NULL, ..., subquery = FALSE) {
  from_x <- sql_render(query$x, con, ..., subquery = FALSE)
  from_y <- sql_render(query$y, con, ..., subquery = FALSE)

  if (dbplyr_edition(con) >= 2) {
    sql_query_set_op(con, from_x, from_y, method = query$type, all = query$all)
  } else {
    if (isTRUE(query$all)) {
      abort("`all` argument not supported by this backend")
    }
    dbplyr_query_set_op(con, from_x, from_y, method = query$type)
  }
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
  op
}
