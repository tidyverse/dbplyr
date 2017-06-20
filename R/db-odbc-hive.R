#' @export
sql_translate_env.Hive <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      var       = sql_prefix("VARIANCE"),
      cot       = function(x){
                    build_sql("1 / TAN(", x, ")")
                  }) ,
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
db_analyze.Hive <- function(con, table, ...) {
  # Using ANALYZE TABLE instead of ANALYZE as recommended in this article: https://cwiki.apache.org/confluence/display/Hive/StatsDev
  sql <- build_sql(
    "ANALYZE TABLE ",
    ident(table),
    " COMPUTE STATISTICS"
    , con = con)
  DBI::dbExecute(con, sql)
}
