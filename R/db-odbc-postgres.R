#' @export
sql_translate_env.PostgreSQL <- function(con) {

    # Started with a copy of the existing PostgreSQL translation
    # and included additional variances that did not pass the
    # tests from dbtest.  To leverage the rest of the translations,
    # the bases are set to base_scalar, base_agg and base_win,
    # instead of base_odbc_...

    sql_variant(
    sql_translator(.parent = base_scalar,
       # cot, cosh, sinh, coth and tanh calculations are based on this article
       # https://en.wikipedia.org/wiki/Hyperbolic_function
       cot = function(x){
         build_sql("1 / TAN(", x, ")")
       },
      cosh = function(x){
        build_sql("(EXP(", x, ") + EXP(-", x,")) / 2")
      },
      sinh = function(x){
        build_sql("(EXP(", x, ") - EXP(-", x,")) / 2")
      },
      tanh = function(x){
        build_sql("((EXP(", x, ") - EXP(-", x,")) / 2) / ((EXP(", x, ") + EXP(-", x,")) / 2)")
      },
      coth = function(x){
        build_sql("((EXP(", x, ") + EXP(-", x,")) / 2) / ((EXP(", x, ") - EXP(-", x,")) / 2)")
      },
      round = function(x, digits = 0L){
        build_sql(
          "ROUND(", x, ", ", as.integer(digits),")"
        )},
      log = function(x, base = exp(1)) {
        if (isTRUE(all.equal(base, exp(1)))) {
          build_sql("ln(", x, ")")
        } else {
          # Use log change-of-base because postgres doesn't support the
          # two-argument "log(base, x)" for floating point x.
          build_sql("log(", x, ") / log(", base, ")")
        }
      },
      paste = function(..., sep = " "){
        build_sql(
          "CONCAT_WS(",sep, ", ",escape(c(...), parens = "", collapse = ","),")"
        )
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
