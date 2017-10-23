#' @export
sql_translate_env.Redshift <- function(con) {
  sql_translate_env.PostgreSQLConnection(con)
}
