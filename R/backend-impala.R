#' @export
sql_translate_env.Impala <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
      bitwNot       = sql_prefix("BITNOT", 1),
      bitwAnd       = function(x, y) sql_prefix("BITAND", 2)(x, as.integer(y)),
      bitwOr        = function(x, y) sql_prefix("BITOR", 2)(x, as.integer(y)),
      bitwXor       = function(x, y) sql_prefix("BITXOR", 2)(x, as.integer(y)),
      bitwShiftL    = function(x, y) sql_prefix("SHIFTLEFT", 2)(x, as.integer(y)),
      bitwShiftR    = function(x, y) sql_prefix("SHIFTRIGHT", 2)(x, as.integer(y)),

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
