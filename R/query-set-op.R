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
sql_render.set_op_query <- function(
  query,
  con = NULL,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  sub_lvl <- lvl + !inherits(con, "SQLiteConnection")
  from_x <- sql_render(query$x, con, ..., subquery = FALSE, lvl = sub_lvl)
  from_y <- sql_render(query$y, con, ..., subquery = FALSE, lvl = sub_lvl)

  sql_query_set_op(
    con,
    from_x,
    from_y,
    method = query$type,
    all = query$all,
    lvl = lvl
  )
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
sql_render.union_query <- function(
  query,
  con = NULL,
  ...,
  sql_options = NULL,
  subquery = FALSE,
  lvl = 0
) {
  from_x <- sql_render(query$x, con, ..., subquery = FALSE, lvl = lvl)
  unions <- list()
  unions$table <- purrr::map(
    query$unions$table,
    \(table) sql_render(table, con, ..., subquery = FALSE, lvl = lvl)
  )
  unions$all <- query$unions$all

  sql_query_union(con, from_x, unions, lvl = lvl)
}

#' @export
flatten_query.set_op_query <- flatten_query_2_tables

#' @export
flatten_query.union_query <- function(qry, query_list, con) {
  x <- qry$x
  query_list_new <- flatten_query(x, query_list, con)
  qry$x <- get_subquery_name(x, query_list_new)

  for (i in seq_along(qry$unions$table)) {
    y <- qry$unions$table[[i]]
    query_list_new <- flatten_query(y, query_list_new, con)
    qry$unions$table[[i]] <- get_subquery_name(y, query_list_new)
  }

  # TODO reuse query
  name <- as_table_path(unique_subquery_name(), con)
  wrapped_query <- set_names(list(qry), name)

  query_list$queries <- c(query_list_new$queries, wrapped_query)
  query_list$name <- name
  query_list
}
