#' @export
#' @rdname sql_build
set_op_query <- function(x, y, type = type) {
  structure(
    list(
      x = x,
      y = y,
      type = type
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
sql_render.set_op_query <- function(query, con = NULL, ..., bare_identifier_ok = FALSE) {
  from_x <- sql_render(query$x, con, ..., bare_identifier_ok = FALSE)
  from_y <- sql_render(query$y, con, ..., bare_identifier_ok = FALSE)

  sql_set_op(con, from_x, from_y, method = query$type)
}

# SQL generation ----------------------------------------------------------

#' @export
sql_set_op.default <- function(con, x, y, method) {
  build_sql(
    "(", x, ")",
    "\n", sql(method), "\n",
    "(", y, ")",
    con = con
  )
}

#' @export
sql_set_op.SQLiteConnection <- function(con, x, y, method) {
  # SQLite does not allow parentheses
  build_sql(
    x,
    "\n", sql(method), "\n",
    y,
    con = con
  )
}
