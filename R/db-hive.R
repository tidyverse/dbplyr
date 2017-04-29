#' @export
db_desc.Hive <- function(x) {
  info <- DBI::dbGetInfo(x)

  host <- if (info$servername == "") "localhost" else info$servername
  port <- if (info$port == "") "" else paste0(":", port)

  paste0(
    info$dbms.name, " ", info$db.version,
    "[", info$username, "@", host, port,
    "/", info$dbname , "]")

}


#' @export
sql_translate_env.Hive <- function(con) {

  sql_variant(
    scalar = dbplyr::sql_translator(
      .parent = dbplyr::base_scalar,
      as.numeric = function(x) build_sql("CAST(", x, " AS DOUBLE)"),
      as.double  = function(x) build_sql("CAST(", x, " AS DOUBLE)"),
      as.integer  = function(x) build_sql("CAST(", x, " AS INT)"),
      as.logical = function(x) build_sql("CAST(", x, " AS BOOLEAN)"),
      as.character  = function(x) build_sql("CAST(", x, " AS STRING)"),
      as.Date  = function(x) build_sql("CAST(", x, " AS DATE)"),
      round = function(x, digits = 0L) build_sql("ROUND(", x, ",", digits,")"),
      paste0 = function(...) build_sql("CONCAT", list(...))
    ) ,
    sql_translator(
      .parent = base_agg,
      n = function() sql("COUNT(*)"),
      count = function() sql("COUNT(*)")
    ),
    sql_translator(
      .parent = base_win,
      n = function() sql("COUNT(*)"),
      count = function() sql("COUNT(*)")
    )
  )
  }


#' @export
sql_escape_ident.Hive <- function(con, x) {
    sql_quote(x, " ")
}



