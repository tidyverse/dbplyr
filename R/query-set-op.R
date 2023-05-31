#' @export
#' @rdname sql_build
set_op_query <- function(x, y, type, all = FALSE) {
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
  cat_line(indent_print(x$x))

  cat_line("Y:")
  cat_line(indent_print(x$y))
}

#' @export
sql_render.set_op_query <- function(query,
                                    con = NULL,
                                    ...,
                                    subquery = FALSE,
                                    lvl = 0) {
  sub_lvl <- lvl + !inherits(con, "SQLiteConnection")
  from_x <- sql_render(query$x, con, ..., subquery = FALSE, lvl = sub_lvl)
  from_y <- sql_render(query$y, con, ..., subquery = FALSE, lvl = sub_lvl)

  if (dbplyr_edition(con) >= 2) {
    sql_query_set_op(
      con,
      from_x,
      from_y,
      method = query$type,
      all = query$all,
      lvl = lvl
    )
  } else {
    # nocov start
    if (isTRUE(query$all)) {
      cli_abort("{.arg all} argument not supported by this backend")
    }
    dbplyr_query_set_op(con, from_x, from_y, method = query$type)
    # nocov end
  }
}

#' @export
#' @rdname sql_build
union_query <- function(x, unions) {
  structure(
    list(
      x = x,
      unions = unions
    ),
    class = c("union_query", "query")
  )
}

#' @export
print.union_query <- function(x, ...) {
  cat_line(indent_print(x$x))

  for (i in seq_along(x$unions$table)) {
    if (x$unions$all[[i]]) {
      method <- "UNION ALL"
    } else {
      method <- "UNION"
    }
    cat_line()
    cat_line(indent(sql(method)))
    cat_line()

    cat_line(indent_print(x$unions$table[[i]]))
  }
}

#' @export
sql_render.union_query <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0) {
  from_x <- sql_render(query$x, con, ..., subquery = FALSE, lvl = lvl)
  unions <- list()
  unions$table <- purrr::map(
    query$unions$table,
    function(table) sql_render(table, con, ..., subquery = FALSE, lvl = lvl)
  )
  unions$all <- query$unions$all

  sql_query_union(con, from_x, unions, lvl = lvl)
}
