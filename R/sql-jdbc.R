#' @export
sql_translate_env.JDBCConnection <- function(x, ...) {
  class <- x@jc

  if (grepl("oracle.jdbc.driver", class)) {
    sql_translate_env.Oracle(x, ...)
  } else {
    sql_translate_env.DBIConnection(x, ...)
  }

}

#' @export
sql_select.JDBCConnection <- function(x, ...) {
  class <- x@jc

  if (grepl("oracle.jdbc.driver", class)) {
    sql_select.Oracle(x, ...)
  } else {
    sql_select.DBIConnection(x, ...)
  }

}

#' @export
db_analyze.JDBCConnection <- function(x, ...) {
  class <- x@jc

  if (grepl("oracle.jdbc.driver", class)) {
    db_analyze.Oracle(x, ...)
  } else {
    db_analyze.DBIConnection(x, ...)
  }

}

#' @export
sql_subquery.JDBCConnection <- function(x, ...) {
  class <- x@jc

  if (grepl("oracle.jdbc.driver", class)) {
    sql_subquery.Oracle(x, ...)
  } else {
    sql_subquery.DBIConnection(x, ...)
  }

}


