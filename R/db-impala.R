#' @export
sql_translate_env.Impala <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
      as.Date  = function(x) build_sql("CAST(", x, " AS VARCHAR(10))")
    ) ,
    base_odbc_agg,
    base_odbc_win
  )
  }



