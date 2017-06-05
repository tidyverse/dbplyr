#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_scalar <- sql_translator(
  .parent = base_scalar,
  as.numeric    = sql_cast("DOUBLE"),
  as.double     = sql_cast("DOUBLE"),
  as.integer    = sql_cast("INT"),
  as.logical    = sql_cast("BOOLEAN"),
  as.character  = sql_cast("STRING"),
  as.Date       = sql_cast("DATE"),
  round         = function(x, digits = 0L)
                    build_sql(
                      "ROUND(", x, ", ", as.integer(digits),")"
                      ),
  paste0        = sql_prefix("CONCAT"),
  paste         = function(..., sep = " ")
                    build_sql(
                      "CONCAT_WS(",sep, ", ",escape(c(...), parens = "", collapse = ","),")"
                      )
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_agg <- sql_translator(
  .parent = base_agg,
  n             = function() sql("COUNT(*)"),
  count         = function() sql("COUNT(*)")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_win <- sql_translator(
  .parent = base_win,
  n             = function() sql("COUNT(*)"),
  count         = function() sql("COUNT(*)"),
  first         = function(x, order = NULL) {
    win_over(
      build_sql("first_value", list(x)),
      win_current_group(),
      win_order_by(x, order = order)
    )
    }
)

#' @export
db_desc.OdbcConnection <- function(x) {
  info <- DBI::dbGetInfo(x)

  host <- if (info$servername == "") "localhost" else info$servername
  port <- if (info$port == "") "" else paste0(":", port)

  paste0(
    info$dbms.name, " ", info$db.version,
    "[", info$username, "@", host, port,
    "/", info$dbname , "]")

}

#' @export
sql_translate_env.OdbcConnection <- function(con) {
  sql_variant(
    base_odbc_scalar,
    base_odbc_agg,
    base_odbc_win
  )
}


#' @export
db_drop_table.OdbcConnection <- function(con, table, force = FALSE, ...) {
  sql <- dbplyr::build_sql(
    "DROP TABLE ", if (force) dbplyr::sql("IF EXISTS "), dbplyr::sql(table),
    con = con
  )
  DBI::dbExecute(con, sql)
}


#' @export
db_copy_to.OdbcConnection <- function(con, table, values,
                                      overwrite = FALSE, types = NULL, temporary = FALSE,
                                      unique_indexes = NULL, indexes = NULL,
                                      analyze = TRUE, ...) {

  if (overwrite) {
    db_drop_table(con, table, force = TRUE)
  }

  dbWriteTable(con, table, values, temporary = temporary)

  if (analyze) db_analyze(con, table)

  table
}

win_order_by <- function(x, order = NULL){

  if(!is.null(order)) order else
    if(!is.null(win_current_order())) win_current_order() else
      if(!is.null(win_current_group())) win_current_group() else
        x

}
