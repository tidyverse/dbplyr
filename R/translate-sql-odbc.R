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
  first = function(x, order = NULL) {
    win_over(
      build_sql("FIRST_VALUE", list(x)),
      win_current_group(),
      order %||% x
    )
  },
  last = function(x, order = NULL) {
    win_over(
      build_sql("FIRST_VALUE", list(x)),
      win_current_group(),
      build_sql(order %||% x, " DESC")
    )
  },
  lead = function(x, n = 1L, order = NULL) {
    n <- as.integer(n)
    win_over(
      build_sql("LEAD (", x, ", ", n, ")"),
      win_current_group(),
      order %||% x
    )
  },
  lag = function(x, n = 1L, order = NULL) {
    n <- as.integer(n)
    win_over(
      build_sql("LAG (", x, ", ", n, ")"),
      win_current_group(),
      order %||% x
      )
  },
  dense_rank = function(x) {
    win_over(
      build_sql("DENSE_RANK () "),
      win_current_group(),
      x
    )
  },
  ntile = function(x, n = 1L) {
    n <- as.integer(n)
    win_over(
      build_sql("NTILE (", n, ") "),
      win_current_group(),
      x
    )
  },
  min_rank = function(x) {
    win_over(
      build_sql("RANK() "),
      win_current_group(),
      x
    )
  },
  row_number = function(x) {
    win_over(
      build_sql("ROW_NUMBER () "),
      win_current_group(),
      x
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
