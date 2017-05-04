#' @export
sql_translate_env.Impala <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
      as.Date  = sql_cast("VARCHAR(10)")
    ) ,
    base_odbc_agg,
    base_odbc_win
  )
}



