
#' @export
#' @rdname db_copy_to
db_compute <- function(con,
                      table,
                      sql,
                      temporary = TRUE,
                      unique_indexes = list(),
                      indexes = list(),
                      analyze = TRUE,
                      ...) {
  UseMethod("db_compute")
}

#' @export
db_compute.DBIConnection <- function(con,
                                     table,
                                     sql,
                                     temporary = TRUE,
                                     unique_indexes = list(),
                                     indexes = list(),
                                     analyze = TRUE,
                                     ...) {
  if (!is.list(indexes)) {
    indexes <- as.list(indexes)
  }
  if (!is.list(unique_indexes)) {
    unique_indexes <- as.list(unique_indexes)
  }

  table <- db_save_query(con, sql, table, temporary = temporary)
  db_create_indexes(con, table, unique_indexes, unique = TRUE)
  db_create_indexes(con, table, indexes, unique = FALSE)
  if (analyze) db_analyze(con, table)

  table
}

#' @export
#' @rdname db_copy_to
db_collect <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  UseMethod("db_collect")
}

#' @export
db_collect.DBIConnection <- function(con, sql, n = -1, warn_incomplete = TRUE, ...) {
  res <- dbSendQuery(con, sql)
  tryCatch({
    out <- dbFetch(res, n = n)
    if (warn_incomplete) {
      res_warn_incomplete(res, "n = Inf")
    }
  }, finally = {
    dbClearResult(res)
  })

  out
}

#' @rdname db_copy_to
#' @export
db_sql_render <- function(con, sql, ...) {
  UseMethod("db_sql_render")
}

#' @export
db_sql_render.DBIConnection <- function(con, sql, ...) {
  qry <- sql_build(sql, con = con, ...)
  sql_render(qry, con = con, ...)
}

