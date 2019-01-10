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
  cat(
    "<SQL ",
    if (x$anti) "ANTI" else "SEMI", " JOIN>\n",
    sep = ""
  )
  cat("By:   ", paste0(x$by$x, "-", x$by$y, collapse = ", "), "\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}

#' @export
sql_render.semi_join_query <- function(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_subquery(con, sql_render(query$x, con, ..., root = root), name = "TBL_LEFT")
  from_y <- sql_subquery(con, sql_render(query$y, con, ..., root = root), name = "TBL_RIGHT")

  sql_semi_join(con, from_x, from_y, anti = query$anti, by = query$by)
}

# SQL generation ----------------------------------------------------------

#' @export
sql_semi_join.DBIConnection <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  # X and Y are subqueries named TBL_LEFT and TBL_RIGHT
  left <- escape(ident("TBL_LEFT"), con = con)
  right <- escape(ident("TBL_RIGHT"), con = con)
  on <- sql_vector(
    paste0(
      left,  ".", sql_escape_ident(con, by$x), " = ",
      right, ".", sql_escape_ident(con, by$y)
    ),
    collapse = " AND ",
    parens = TRUE,
    con = con
  )

  build_sql(
    "SELECT * FROM ", x, "\n\n",
    "WHERE ", if (anti) sql("NOT "), "EXISTS (\n",
    "  SELECT 1 FROM ", y, "\n",
    "  WHERE ", on, "\n",
    ")",
    con = con
  )
}

