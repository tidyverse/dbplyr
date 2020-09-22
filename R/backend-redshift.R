#' @export
sql_translate_env.RedshiftConnection <- function(con) {
  postgres <- sql_translate_env.PostgreSQL(con)

  sql_variant(
    sql_translator(.parent = postgres$scalar,

      # https://docs.aws.amazon.com/redshift/latest/dg/r_Numeric_types201.html#r_Numeric_types201-floating-point-types
      as.numeric = sql_cast("FLOAT"),
      as.double = sql_cast("FLOAT"),

      # https://stackoverflow.com/questions/56708136
      paste  = sql_paste_redshift(" "),
      paste0 = sql_paste_redshift(""),
      str_c = sql_paste_redshift(""),

      # https://docs.aws.amazon.com/redshift/latest/dg/r_SUBSTRING.html
      substr = sql_substr("SUBSTRING"),
      str_sub = sql_str_sub("SUBSTRING", "LEN"),

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

sql_paste_redshift <- function(sep) {
  sql_paste_infix(sep, "||", function(x) sql_expr(cast(!!x %as% text)))
}
