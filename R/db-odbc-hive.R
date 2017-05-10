#' @export
sql_translate_env.Hive <- function(con) {
  sql_variant(
    base_odbc_scalar,
    base_odbc_agg,
    base_odbc_win
  )
}
