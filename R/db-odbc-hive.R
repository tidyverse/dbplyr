#' @export
sql_translate_env.Hive <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
      var       = sql_prefix("VARIANCE")
    ) ,
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
db_analyze.Hive <- function(con, table, ...) {
  # Using ANALYZE TABLE instead of ANALYZE as recommended in this article
  # https://cwiki.apache.org/confluence/display/Hive/StatsDev
  sql <- dbplyr::build_sql(
    "ANALYZE TABLE ",
    dbplyr::ident(table),
    " COMPUTE STATISTICS"
    , con = con)
  DBI::dbExecute(con, sql)
}
