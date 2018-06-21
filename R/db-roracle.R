#' @export
sql_translate_env.OraConnection <- function(con) {
  sql_translate_env.Oracle(con)
}

#' @export
sql_select.OraConnection <- function(con, select, from, where = NULL,
                             group_by = NULL, having = NULL,
                             order_by = NULL,
                             limit = NULL,
                             distinct = FALSE,
                             ...) {
  sql_select.Oracle(con, select, from, where = where,
                               group_by = group_by, having = having,
                               order_by = order_by,
                               limit = limit,
                               distinct = distinct,
                               ...)
}

#' @export
db_analyze.OraConnection <- function(con, table, ...) {
  db_analyze.Oracle(con = con, table = table, ...)
}

#' @export
sql_subquery.OraConnection <- function(con, from, name = unique_name(), ...) {
  sql_subquery.Oracle(con = con, from = from, name = name, ...)
}

#' @export
db_drop_table.OraConnection <- function(con, table, force = FALSE, ...) {
  db_drop_table.Oracle(con = con, table = table, force = force, ...)
}

# registered onLoad located in the zzz.R script
setdiff.OraConnection<- function(x, y, copy = FALSE, ...) {
  setdiff.tbl_Oracle(x = x, y = y, copy = copy, ...)
}
