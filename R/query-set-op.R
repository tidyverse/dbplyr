#' @export
#' @rdname sql_build
lazy_set_op_query <- function(x, y, type, all, call = caller_env()) {
  check_lazy_query(x, call = call)
  check_lazy_query(y, call = call)
  check_string(type, call = call)
  check_bool(all, call = call)

  lazy_query(
    query_type = "set_op",
    x = x,
    y = y,
    type = type,
    all = all
  )
}

#' @export
op_vars.lazy_set_op_query <- function(op) {
  union(op_vars(op$x), op_vars(op$y))
}

#' @export
sql_build.lazy_set_op_query <- function(op, con, ..., sql_options = NULL) {
  # add_op_set_op() ensures that both have same variables
  set_op_query(
    sql_build(op$x, con, sql_options = sql_options),
    sql_build(op$y, con, sql_options = sql_options),
    type = op$type,
    all = op$all
  )
}

#' @export
#' @rdname sql_build
lazy_union_query <- function(x, unions, call = caller_env()) {
  check_lazy_query(x, call = call)

  lazy_query(
    query_type = "union",
    x = x,
    unions = unions
  )
}

#' @export
op_vars.lazy_union_query <- function(op) {
  purrr::reduce(
    op$unions$table,
    \(acc, table) union(acc, op_vars(table$lazy_query)),
    .init = op_vars(op$x)
  )
}

#' @export
sql_build.lazy_union_query <- function(op, con, ..., sql_options = NULL) {
  # add_union() ensures that both have same variables
  unions <- list(
    table = purrr::map(
      op$unions$table,
      function(table) {
        sql_build(table, con, sql_options = sql_options)
      }
    ),
    all = op$unions$all
  )

  union_query(
    sql_build(op$x, con, sql_options = sql_options),
    unions
  )
}

# Built query -------------------------------------------------------------

#' @export
#' @rdname sql_build
set_op_query <- function(x, y, type, all = FALSE) {
  query("set_op", x = x, y = y, type = type, all = all)
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
  query("union", x = x, unions = unions)
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
flatten_query.set_op_query <- function(qry, query_list, con) {
  flatten_query_2_tables(qry, query_list, con)
}

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
