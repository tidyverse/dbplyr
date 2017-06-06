#' @export
sql_translate_env.PostgreSQL <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      log = function(x, base = exp(1)) {
        if (isTRUE(all.equal(base, exp(1)))) {
          build_sql("ln(", x, ")")
        } else {
          # Use log change-of-base because postgres doesn't support the
          # two-argument "log(base, x)" for floating point x.
          build_sql("log(", x, ") / log(", base, ")")
        }
      }
    ),
    sql_translator(.parent = base_agg,
      n = function() sql("count(*)"),
      cor = sql_prefix("corr"),
      cov = sql_prefix("covar_samp"),
      sd =  sql_prefix("stddev_samp"),
      var = sql_prefix("var_samp"),
      all = sql_prefix("bool_and"),
      any = sql_prefix("bool_or"),
      paste = function(x, collapse) build_sql("string_agg(", x, ", ", collapse, ")")
    ),
    base_win
  )
}
