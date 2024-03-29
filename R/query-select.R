#' @export
#' @rdname sql_build
select_query <- function(from,
                         select = sql("*"),
                         where = character(),
                         group_by = character(),
                         having = character(),
                         window = character(),
                         order_by = character(),
                         limit = NULL,
                         distinct = FALSE,
                         from_alias = NULL) {
  check_character(select)
  check_character(where)
  check_character(group_by)
  check_character(having)
  check_character(window)
  check_character(order_by)
  check_number_whole(limit, allow_infinite = TRUE, allow_null = TRUE)
  check_bool(distinct)
  check_string(from_alias, allow_null = TRUE)

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      having = having,
      window = window,
      order_by = order_by,
      distinct = distinct,
      limit = limit,
      from_alias = from_alias
    ),
    class = c("select_query", "query")
  )
}

#' @export
print.select_query <- function(x, ...) {
  cat_line("<SQL SELECT", if (x$distinct) " DISTINCT", ">")
  cat_line("From:")
  cat_line(indent_print(x$from))

  if (length(x$select))   cat_line("Select:   ", named_commas(x$select))
  if (length(x$where))    cat_line("Where:    ", named_commas(x$where))
  if (length(x$group_by)) cat_line("Group by: ", named_commas(x$group_by))
  if (length(x$window))   cat_line("Window:   ", named_commas(x$window))
  if (length(x$order_by)) cat_line("Order by: ", named_commas(x$order_by))
  if (length(x$having))   cat_line("Having:   ", named_commas(x$having)) # nocov
  if (length(x$limit))    cat_line("Limit:    ", x$limit)
}

#' @export
sql_optimise.select_query <- function(x, con = NULL, ..., subquery = FALSE) {
  if (!inherits(x$from, "select_query")) {
    return(x)
  }

  from <- sql_optimise(x$from, subquery = subquery)

  # If all outer clauses are executed after the inner clauses, we
  # can drop them down a level
  outer <- select_query_clauses(x, subquery = subquery)
  inner <- select_query_clauses(from, subquery = TRUE)

  can_squash <- length(outer) == 0 || length(inner) == 0 || min(outer) > max(inner)

  if (can_squash) {
    # Have we optimised away an ORDER BY
    if (length(from$order_by) > 0 && length(x$order_by) > 0) {
      if (!identical(x$order_by, from$order_by)) {
        warn_drop_order_by()
      }
    }

    from[as.character(outer)] <- x[as.character(outer)]
    from
  } else {
    x$from <- from
    x
  }
}

# List clauses used by a query, in the order they are executed
# https://sqlbolt.com/lesson/select_queries_order_of_execution

# List clauses used by a query, in the order they are executed in
select_query_clauses <- function(x, subquery = FALSE) {
  present <- c(
    where    = length(x$where) > 0,
    group_by = length(x$group_by) > 0,
    having   = length(x$having) > 0,
    select   = !identical(unname(x$select), sql("*")),
    distinct = x$distinct,
    window   = length(x$window) > 0,
    order_by = (!subquery || !is.null(x$limit)) && length(x$order_by) > 0,
    limit    = !is.null(x$limit)
  )

  ordered(names(present)[present], levels = names(present))
}

#' @export
sql_render.select_query <- function(query,
                                    con,
                                    ...,
                                    sql_options = NULL,
                                    subquery = FALSE,
                                    lvl = 0) {
  from <- dbplyr_sql_subquery(con,
    sql_render(query$from, con, ..., subquery = TRUE, lvl = lvl + 1),
    name = query$from_alias,
    lvl = lvl
  )

  dbplyr_query_select(
    con, query$select, from,
    where = query$where,
    group_by = query$group_by,
    having = query$having,
    window = query$window,
    order_by = query$order_by,
    limit = query$limit,
    distinct = query$distinct,
    ...,
    subquery = subquery,
    lvl = lvl
  )
}

warn_drop_order_by <- function() {
  # Rules according to SQLite: https://sqlite.org/forum/forumpost/878ca7a9be0862af?t=h
  # 1. There is no LIMIT clause in the subquery
  # 3. The subquery is not part of the FROM clause in an UPDATE-FROM statement
  # 4. The outer query does not use any aggregate functions other than the built-in count(), min(), and/or max() functions.
  # 5. Either the outer query has its own ORDER BY clause or else the subquery is one term of a join.
  #
  # (2. and 6. are left out as they are SQLite internal)
  warn(c(
    "ORDER BY is ignored in subqueries without LIMIT",
    i = "Do you need to move arrange() later in the pipeline or use window_order() instead?"
  ))
}

#' @export
flatten_query.select_query <- function(qry, query_list, con) {
  from <- qry$from
  query_list <- flatten_query(from, query_list, con)

  qry$from <- get_subquery_name(from, query_list)
  querylist_reuse_query(qry, query_list, con)
}
