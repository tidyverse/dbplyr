#' Build and render SQL from a sequence of lazy operations
#'
#' `sql_build()` creates a `select_query` S3 object, that is rendered
#' to a SQL string by `sql_render()`. The output from `sql_build()` is
#' designed to be easy to test, as it's database agnostic, and has
#' a hierarchical structure. Outside of testing, however, you should
#' always call `sql_render()`.
#'
#' `sql_build()` is generic over the lazy operations, \link{lazy_ops},
#' and generates an S3 object that represents the query. `sql_render()`
#' takes a query object and then calls a function that is generic
#' over the database. For example, `sql_build.op_mutate()` generates
#' a `select_query`, and `sql_render.select_query()` calls
#' `sql_select()`, which has different methods for different databases.
#' The default methods should generate ANSI 92 SQL where possible, so you
#' backends only need to override the methods if the backend is not ANSI
#' compliant.
#'
#' @export
#' @keywords internal
#' @param op A sequence of lazy operations
#' @param con A database connection. The default `NULL` uses a set of
#'   rules that should be very similar to ANSI 92, and allows for testing
#'   without an active database connection.
#' @param ... Other arguments passed on to the methods. Not currently used.
sql_build <- function(op, con = NULL, ..., use_star = TRUE) {
  unique_subquery_name_reset()
  check_dots_used()
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_lazy <- function(op, con = op$src$con, ..., use_star = TRUE) {
  # only used for testing
  qry <- sql_build(op$lazy_query, con = con, ..., use_star = use_star)
  sql_optimise(qry, con = con, ...)
}

#' @export
sql_build.ident <- function(op, con = NULL, ..., use_star = TRUE) {
  op
}


# Render ------------------------------------------------------------------

#' @export
#' @rdname sql_build
#' @param subquery Is this SQL going to be used in a subquery?
#'   This is important because you can place a bare table name in a subquery
#'   and  ORDER BY does not work in subqueries.
sql_render <- function(query,
                       con = NULL,
                       ...,
                       use_star = TRUE,
                       subquery = FALSE,
                       lvl = 0,
                       cte = FALSE) {
  check_dots_used()
  UseMethod("sql_render")
}

#' @export
sql_render.tbl_lazy <- function(query,
                                con = query$src$con,
                                ...,
                                use_star = TRUE,
                                subquery = FALSE,
                                lvl = 0,
                                cte = FALSE) {
  sql_render(
    query$lazy_query,
    con = con,
    ...,
    use_star = use_star,
    subquery = subquery,
    lvl = lvl,
    cte = cte
  )
}

#' @export
sql_render.lazy_query <- function(query,
                                  con = NULL,
                                  ...,
                                  use_star = TRUE,
                                  subquery = FALSE,
                                  lvl = 0,
                                  cte = FALSE) {
  qry <- sql_build(query, con = con, ..., use_star = use_star)
  qry <- sql_optimise(qry, con = con, ..., subquery = subquery)

  if (cte) {
    if (is.ident(qry) || is.sql(qry)) {
      return(sql_render(qry, con))
    }

    query_list <- flatten_query(qry, list(queries = list(), name = NULL))
    queries <- query_list$queries

    rendered_queries <- purrr::map2(
      queries, seq_along(queries) != length(queries),
      function(query, indent) {
        sql_render(query, con = con, ..., subquery = subquery, lvl = as.integer(indent))
      }
    )

    cte_render(rendered_queries, con)
  } else {
    sql_render(qry, con = con, ..., subquery = subquery, lvl = lvl)
  }
}

cte_render <- function(query_list, con) {
  n <- length(query_list)
  if (n == 1) {
    return(query_list[[1]])
  }

  ctes <- purrr::imap(query_list[-n],
    function(query, name) {
      glue_sql2(con, "{.name `name`} {.kw 'AS'} (\n{.sql query}\n)")
    }
  )
  cte_query <- sql_vector(unname(ctes), parens = FALSE, collapse = ",\n", con = con)

  glue_sql2(con, "{.kw 'WITH'} ", cte_query, "\n", query_list[[n]])
}

get_subquery_name <- function(x, query_list) {
  if (is.ident(x) || is.sql(x)) return(x)

  ident(query_list$name)
}

flatten_query <- function(qry, query_list) {
  UseMethod("flatten_query")
}

#' @export
flatten_query.select_query <- function(qry, query_list) {
  from <- qry$from
  query_list <- flatten_query(from, query_list)

  qry$from <- get_subquery_name(from, query_list)
  querylist_reuse_query(qry, query_list)
}

querylist_reuse_query <- function(qry, query_list) {
  id <- vctrs::vec_match(list(unclass(qry)), purrr::map(query_list$queries, unclass))

  if (!is.na(id)) {
    query_list$name <- names(query_list$queries)[[id]]
  } else {
    name <- unique_subquery_name()
    wrapped_query <- set_names(list(qry), name)
    query_list$queries <- c(query_list$queries, wrapped_query)
    query_list$name <- name
  }

  query_list
}

#' @export
flatten_query.join_query <- function(qry, query_list) {
  x <- qry$x
  query_list_x <- flatten_query(x, query_list)
  qry$x <- get_subquery_name(x, query_list_x)

  y <- qry$y
  query_list_y <- flatten_query(y, query_list_x)
  qry$y <- get_subquery_name(y, query_list_y)

  querylist_reuse_query(qry, query_list_y)
}

#' @export
flatten_query.multi_join_query <- function(qry, query_list) {
  x <- qry$x
  query_list_new <- flatten_query(x, query_list)
  qry$x <- get_subquery_name(x, query_list_new)

  for (i in vctrs::vec_seq_along(qry$joins)) {
    y <- qry$joins$table[[i]]
    query_list_new <- flatten_query(y, query_list_new)
    qry$joins$table[[i]] <- get_subquery_name(y, query_list_new)
  }

  # TODO reuse query
  name <- unique_subquery_name()
  wrapped_query <- set_names(list(qry), name)

  query_list$queries <- c(query_list_new$queries, wrapped_query)
  query_list$name <- name
  query_list
}

#' @export
flatten_query.semi_join_query <- flatten_query.join_query
#' @export
flatten_query.set_op_query <- flatten_query.join_query

#' @export
flatten_query.ident <- function(qry, query_list) {
  query_list
}

#' @export
flatten_query.sql <- function(qry, query_list) {
  query_list
}

#' @export
sql_render.sql <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0, cte = FALSE) {
  query
}

#' @export
sql_render.ident <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0, cte = FALSE) {
  if (subquery) {
    query
  } else {
    dbplyr_query_select(con, sql("*"), query, lvl = lvl)
  }
}

#' @export
sql_render.dbplyr_schema <- function(query, con = NULL, ..., subquery = FALSE, lvl = 0, cte = FALSE) {
  query <- as.sql(query, con)
  sql_render(query, con = con, ..., subquery = subquery, lvl = lvl, cte = cte)
}

#' @export
sql_render.dbplyr_catalog <- sql_render.dbplyr_schema

# Optimise ----------------------------------------------------------------

#' @export
#' @rdname sql_build
sql_optimise <- function(x, con = NULL, ..., subquery = FALSE) {
  UseMethod("sql_optimise")
}

#' @export
sql_optimise.sql <- function(x, con = NULL, ...) {
  # Can't optimise raw SQL
  x
}

#' @export
sql_optimise.ident <- function(x, con = NULL, ...) {
  x
}
