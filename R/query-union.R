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

# SQL generation ----------------------------------------------------------

#' @rdname db-sql
#' @export
sql_query_union <- function(con, x, unions, ..., lvl = 0) {
  dialect <- sql_dialect(con)
  return(sql_query_union_(dialect, x, unions, ..., lvl = lvl))

  UseMethod("sql_query_union")
}
sql_query_union_ <- function(dialect, x, unions, ..., lvl = 0) {
  UseMethod("sql_query_union")
}
#' @export
sql_query_union.DBIConnection <- function(con, x, unions, ..., lvl = 0) {
  ops <- ifelse(unions$all, "UNION ALL", "UNION")
  ops <- purrr::map_chr(ops, \(op) sql_set_op_method(con, op))
  ops <- indent_lvl(style_kw(ops), lvl)
  tables <- unlist(unions$table)

  union_clauses <- vctrs::vec_interleave(as.character(ops), tables)
  out <- paste0(
    x,
    "\n\n",
    paste0(union_clauses, collapse = "\n\n")
  )

  sql(out)
}

#' @export
sql_query_union.sql_dialect <- sql_query_union.DBIConnection
