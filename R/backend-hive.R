#' @export
sql_translate_env.Hive <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      bitwShiftL    = sql_prefix("SHIFTLEFT", 2),
      bitwShiftR    = sql_prefix("SHIFTRIGHT", 2),

      cot = function(x){
        sql_expr(1 / tan(!!x))
      },

      str_replace_all = function(string, pattern, replacement) {
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
      }
    ),
    sql_translator(.parent = base_odbc_agg,
      var = sql_prefix("VARIANCE"),
      quantile = sql_quantile("PERCENTILE"),
      median = sql_median("PERCENTILE")
    ),
    sql_translator(.parent = base_odbc_agg,
      var = win_aggregate("VARIANCE"),
      quantile = sql_quantile("PERCENTILE", window = TRUE),
      median = sql_median("PERCENTILE", window = TRUE)
    )
  )
}

#' @export
db_analyze.Hive <- function(con, table, ...) {
  # Using ANALYZE TABLE instead of ANALYZE as recommended in this article: https://cwiki.apache.org/confluence/display/Hive/StatsDev
  sql <- build_sql(
    "ANALYZE TABLE ",
    as.sql(table),
    " COMPUTE STATISTICS"
    , con = con)
  DBI::dbExecute(con, sql)
}

globalVariables("regexp_replace")
