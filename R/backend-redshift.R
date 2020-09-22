#' @export
sql_translate_env.RedshiftConnection <- function(con) {
  postgres <- sql_translate_env.PostgreSQL(con)

  sql_variant(
    sql_translator(.parent = postgres$scalar),
    sql_translator(.parent = postgres$aggregate),
    sql_translator(.parent = postgres$window)
  )
}

#' @export
sql_translate_env.Redshift <- sql_translate_env.RedshiftConnection
