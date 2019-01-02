#' More db generics
#'
#' These are new, so not included in dplyr for backward compatibility
#' purposes.
#'
#' @keywords internal
#' @export
db_copy_to <-  function(con, table, values,
                        overwrite = FALSE, types = NULL, temporary = TRUE,
                        unique_indexes = NULL, indexes = NULL,
                        analyze = TRUE, ...) {
  UseMethod("db_copy_to")
}

#' @export
db_copy_to.DBIConnection <- function(con, table, values,
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {

  types <- types %||% db_data_type(con, values)
  names(types) <- names(values)

  db_begin(con)
  tryCatch({
    # Only remove if it exists; returns NA for MySQL
    if (overwrite && !is_false(db_has_table(con, table))) {
      db_drop_table(con, table, force = TRUE)
    }

    table <- db_write_table(con, table, types = types, values = values, temporary = temporary)
    db_create_indexes(con, table, unique_indexes, unique = TRUE)
    db_create_indexes(con, table, indexes, unique = FALSE)
    if (analyze) db_analyze(con, table)

    db_commit(con)
  }, error = function(err) {
    db_rollback(con)
    stop(err)
  })

  table
}

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

