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

  # If all outer clauses are exeucted after the inner clauses, we
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
