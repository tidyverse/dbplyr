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
flatten_query.set_op_query <- function(qry, query_list, con) {
  flatten_query_2_tables(qry, query_list, con)
}
