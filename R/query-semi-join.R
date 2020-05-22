#' @export
#' @rdname sql_build
semi_join_query <- function(x, y, anti = FALSE, by = NULL) {
  structure(
    list(
      x = x,
      y = y,
      anti = anti,
      by = by
    ),
    class = c("semi_join_query", "query")
  )
}

#' @export
print.semi_join_query <- function(x, ...) {
  cat_line("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  cat_line("X:")
  cat_line(indent_print(sql_build(x$x)))

  cat_line("Y:")
  cat_line(indent_print(sql_build(x$y)))
}

#' @export
sql_render.semi_join_query <- function(query, con = NULL, ..., bare_identifier_ok = FALSE) {
  from_x <- sql_subquery(
    con,
    sql_render(query$x, con, ..., bare_identifier_ok = TRUE),
    name = "LHS"
  )
  from_y <- sql_subquery(
    con,
    sql_render(query$y, con, ..., bare_identifier_ok = TRUE),
    name = "RHS"
  )

  sql_semi_join(con, from_x, from_y, anti = query$anti, by = query$by)
}

# SQL generation ----------------------------------------------------------

#' @export
sql_semi_join.DBIConnection <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  lhs <- escape(ident("LHS"), con = con)
  rhs <- escape(ident("RHS"), con = con)

  on <- sql_join_tbls(con, by)

  build_sql(
    "SELECT * FROM ", x, "\n",
    "WHERE ", if (anti) sql("NOT "), "EXISTS (\n",
    "  SELECT 1 FROM ", y, "\n",
    "  WHERE ", on, "\n",
    ")",
    con = con
  )
}

