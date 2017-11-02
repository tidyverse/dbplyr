#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_scalar <- sql_translator(.parent = base_scalar,
  as.numeric    = sql_cast("DOUBLE"),
  as.double     = sql_cast("DOUBLE"),
  as.integer    = sql_cast("INT"),
  as.logical    = sql_cast("BOOLEAN"),
  as.character  = sql_cast("STRING"),
  as.Date       = sql_cast("DATE"),
  paste0        = sql_prefix("CONCAT"),
                  # cosh, sinh, coth and tanh calculations are based on this article
                  # https://en.wikipedia.org/wiki/Hyperbolic_function
  cosh          = function(x) build_sql("(EXP(", x, ") + EXP(-(", x,"))) / 2"),
  sinh          = function(x) build_sql("(EXP(", x, ") - EXP(-(", x,"))) / 2"),
  tanh          = function(x){
                    build_sql(
                      "((EXP(", x, ") - EXP(-(", x,"))) / 2) / ((EXP(", x, ") + EXP(-(", x,"))) / 2)"
                    )},
  round         = function(x, digits = 0L){
                    build_sql(
                      "ROUND(", x, ", ", as.integer(digits),")"
                    )},
  coth          = function(x){
                    build_sql(
                      "((EXP(", x, ") + EXP(-(", x,"))) / 2) / ((EXP(", x, ") - EXP(-(", x,"))) / 2)"
                    )},
  paste         = function(..., sep = " "){
                    build_sql(
                      "CONCAT_WS(",sep, ", ",escape(c(...), parens = "", collapse = ","),")"
                    )}
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_agg <- sql_translator(
  .parent = base_agg,
  n             = function() sql("COUNT(*)"),
  count         = function() sql("COUNT(*)"),
  sd            = sql_prefix("STDDEV_SAMP")
)

#' @export
#' @rdname sql_variant
#' @format NULL
base_odbc_win <- sql_translator(.parent = base_win,
  sd            = win_aggregate("STDDEV_SAMP"),
  count         = function() {
                    win_over(sql("COUNT(*)"), win_current_group())
                    }
)

#' @export
db_desc.OdbcConnection <- function(x) {
  info <- DBI::dbGetInfo(x)

  host <- if (info$servername == "") "localhost" else info$servername
  port <- if (info$port == "") "" else paste0(":", info$port)

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
  sql <- build_sql(
    "DROP TABLE ", if (force) sql("IF EXISTS "), sql(table),
    con = con
  )
  DBI::dbExecute(con, sql)
}
