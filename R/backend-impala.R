#' @export
sql_translate_env.Impala <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
      bitwNot       = sql_prefix("BITNOT", 1),
      bitwAnd       = sql_prefix("BITAND", 2),
      bitwOr        = sql_prefix("BITOR", 2),
      bitwXor       = sql_prefix("BITXOR", 2),
      bitwShiftL    = sql_prefix("SHIFTLEFT", 2),
      bitwShiftR    = sql_prefix("SHIFTRIGHT", 2),

      as.Date       = sql_cast("VARCHAR(10)"),
      ceiling       = sql_prefix("CEIL")
    ) ,
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
db_analyze.Impala <- function(con, table, ...) {
  # Using COMPUTE STATS instead of ANALYZE as recommended in this article
  # https://www.cloudera.com/documentation/enterprise/5-9-x/topics/impala_compute_stats.html
  sql <- build_sql("COMPUTE STATS ", as.sql(table), con = con)
  DBI::dbExecute(con, sql)
}
