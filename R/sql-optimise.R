#' @export
#' @rdname sql_build
sql_optimise <- function(x, con = NULL, ...) {
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

#' @export
sql_optimise.query <- function(x, con = NULL, ...) {
  # Default to no optimisation
  x
}

#' @export
sql_optimise.select_query <- function(x, con = NULL, ...) {
  if (!inherits(x$from, "select_query")) {
    return(x)
  }

  from <- sql_optimise(x$from)

  # If all outer clauses are executed after the inner clauses, we
  # can drop them down a level
  outer <- select_query_clauses(x)
  inner <- select_query_clauses(from)

  if (length(outer) == 0 || length(inner) == 0)
    return(x)

  if (min(outer) > max(inner)) {
    from[as.character(outer)] <- x[as.character(outer)]
    from
  } else {
    x
  }
}

# List clauses used by a query, in the order they are executed
# https://sqlbolt.com/lesson/select_queries_order_of_execution

# List clauses used by a query, in the order they are executed in
select_query_clauses <- function(x) {
  present <- c(
    where =    length(x$where) > 0,
    group_by = length(x$group_by) > 0,
    having =   length(x$having) > 0,
    select =   !identical(x$select, sql("*")),
    distinct = x$distinct,
    order_by = length(x$order_by) > 0,
    limit    = !is.null(x$limit)
  )

  ordered(names(present)[present], levels = names(present))
}
