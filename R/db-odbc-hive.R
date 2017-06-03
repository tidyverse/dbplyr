#' @export
sql_translate_env.Hive <- function(con) {
  sql_variant(
    scalar = sql_translator(.parent = base_odbc_scalar,
                            ceiling       = sql_prefix("VARIANCE")
    ) ,
    base_odbc_agg,
    base_odbc_win
  )
}
