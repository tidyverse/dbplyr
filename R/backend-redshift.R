#' @export
sql_translate_env.RedshiftConnection <- function(con) {
  postgres <- sql_translate_env.PostgreSQL(con)

  sql_variant(
    sql_translator(.parent = postgres$scalar,
      # https://docs.aws.amazon.com/redshift/latest/dg/REGEXP_REPLACE.html
      str_replace = sql_not_supported("str_replace"),
      str_replace_all = function(string, pattern, replacement) {
        sql_expr(REGEXP_REPLACE(!!string, !!pattern, !!replacement))
      }
    ),
    sql_translator(.parent = postgres$aggregate),
    sql_translator(.parent = postgres$window)
  )
}

#' @export
sql_translate_env.Redshift <- sql_translate_env.RedshiftConnection
