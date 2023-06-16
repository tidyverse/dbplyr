#' @export
#' @rdname sql_build
semi_join_query <- function(x,
                            y,
                            vars,
                            anti = FALSE,
                            by = NULL,
                            where = NULL,
                            na_matches = FALSE) {
  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      anti = anti,
      by = by,
      where = where,
      na_matches = na_matches
    ),
    class = c("semi_join_query", "query")
  )
}

#' @export
print.semi_join_query <- function(x, ...) {
  cat_line("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>")

  cat_line("By:")
  cat_line(indent(paste0(x$by$x, "-", x$by$y)))

  if (length(x$where)) {
    cat_line("Where:")
    where <- purrr::map_chr(x$where, as_label)
    cat_line(indent(paste0(where)))
  }

  cat_line("X:")
  cat_line(indent_print(x$x))

  cat_line("Y:")
  cat_line(indent_print(x$y))
}

#' @export
sql_render.semi_join_query <- function(query,
                                       con = NULL,
                                       ...,
                                       subquery = FALSE,
                                       lvl = 0) {
  from_x <- sql_render(query$x, con, ..., subquery = TRUE, lvl = lvl + 1)
  from_y <- sql_render(query$y, con, ..., subquery = TRUE, lvl = lvl + 1)

  dbplyr_query_semi_join(
    con,
    from_x,
    from_y,
    vars = query$vars,
    anti = query$anti,
    by = query$by,
    where = query$where,
    lvl = lvl
  )
}
