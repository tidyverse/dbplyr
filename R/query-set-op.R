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
  cat("<SQL ", x$type, ">\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}

#' @export
sql_render.set_op_query <- function(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_render(query$x, con, ..., root = TRUE)
  from_y <- sql_render(query$y, con, ..., root = TRUE)

  sql_set_op(con, from_x, from_y, method = query$type)
}

# SQL generation ----------------------------------------------------------

#' @export
sql_set_op.default <- function(con, x, y, method) {
  build_sql(
    "(", x, ")",
    "\n", sql(method), "\n",
    "(", y, ")"
  )
}

#' @export
sql_set_op.SQLiteConnection <- function(con, x, y, method) {
  # SQLite does not allow parentheses
  build_sql(
    x,
    "\n", sql(method), "\n",
    y
  )
}
