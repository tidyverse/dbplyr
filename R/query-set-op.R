#' @export
#' @rdname sql_build
set_op_query <- function(x, y, type = type, all = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      type = type,
      all = all
    ),
    class = c("set_op_query", "query")
  )
}

#' @export
print.set_op_query <- function(x, ...) {
  cat_line("<SQL ", toupper(x$type), ">")

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x, con = simulate_dbi())))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y, con = simulate_dbi())))
}

#' @export
sql_optimise.set_op_query <- function(x, con = NULL, ..., subquery = FALSE) {
  x$x <- sql_optimise(x$x, con, subquery = FALSE)
  x$y <- sql_optimise(x$y, con, subquery = FALSE)

  x
}

#' @export
sql_render.set_op_query <- function(query, con = NULL, ..., subquery = FALSE) {
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
