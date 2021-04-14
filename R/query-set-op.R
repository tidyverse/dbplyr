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
  cat_line(indent_print(sql_build(x$x)))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y)))
}

#' @export
sql_render.set_op_query <- function(query, con = NULL, ..., subquery = FALSE, level = 0) {
  # TODO improve this hack a little
  sub_level <- level + !inherits(con, "SQLiteConnection")
  from_x <- sql_render(query$x, con, ..., subquery = FALSE, level = sub_level)
  from_y <- sql_render(query$y, con, ..., subquery = FALSE, level = sub_level)

  if (dbplyr_edition(con) >= 2) {
    sql_query_set_op(con, from_x, from_y, method = query$type, all = query$all, level = level)
  } else {
    if (isTRUE(query$all)) {
      abort("`all` argument not supported by this backend")
    }
    dbplyr_query_set_op(con, from_x, from_y, method = query$type, level = level)
  }
}
