#' @include translate-sql-window.R
#' @include translate-sql-helpers.R
#' @include translate-sql-paste.R
#' @include escape.R
NULL

#' @export
sql_translate_env.OdbcConnection <- function(con) {
  sql_variant(
    base_odbc_scalar,
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_scalar <- sql_translator(
  .parent = base_scalar,
  as.numeric = sql_cast("DOUBLE"),
  as.double = sql_cast("DOUBLE"),
  as.integer = sql_cast("INT"),
  as.character = sql_cast("STRING")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_agg <- sql_translator(
  .parent = base_agg,
  n = function() sql("COUNT(*)"),
  count = function() sql("COUNT(*)"),
  sd = sql_prefix("STDDEV_SAMP")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_win <- sql_translator(
  .parent = base_win,
  sd = win_aggregate("STDDEV_SAMP"),
  n = function() win_over(sql("COUNT(*)"), win_current_group()),
  count = function() win_over(sql("COUNT(*)"), win_current_group())
)

#' @export
db_desc.OdbcConnection <- function(x) {
  info <- DBI::dbGetInfo(x)

  host <- if (info$servername == "") "localhost" else info$servername
  port <- if (info$port == "") "" else paste0(":", info$port)

  paste0(
    info$dbms.name, " ", info$db.version,
    "[", info$username, "@", host, port,
    "/", info$dbname, "]"
  )
}

utils::globalVariables("EXP")
