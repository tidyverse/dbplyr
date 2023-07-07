#' Backend: Hive
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are a scattering of custom translations provided by users.
#'
#' Use `simulate_hive()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-hive
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, d = 2, c = "z", con = simulate_hive())
#' lf %>% transmute(x = cot(b))
#' lf %>% transmute(x = bitwShiftL(c, 1L))
#' lf %>% transmute(x = str_replace_all(c, "a", "b"))
#'
#' lf %>% summarise(x = median(d, na.rm = TRUE))
#' lf %>% summarise(x = var(c, na.rm = TRUE))
NULL

#' @export
#' @rdname simulate_dbi
simulate_hive <- function() simulate_dbi("Hive")

#' @export
dbplyr_edition.Hive <- function(con) {
  2L
}

#' @export
sql_translation.Hive <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      bitwShiftL    = sql_prefix("SHIFTLEFT", 2),
      bitwShiftR    = sql_prefix("SHIFTRIGHT", 2),

      cot = function(x){
        sql_expr(1 / tan(!!x))
      },

      str_replace_all = function(string, pattern, replacement) {
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
      }
    ),
    sql_translator(.parent = base_odbc_agg,
      var = sql_aggregate("VARIANCE", "var"),
      quantile = sql_quantile("PERCENTILE"),
      median = sql_median("PERCENTILE")
    ),
    sql_translator(.parent = base_odbc_win,
      var = win_aggregate("VARIANCE"),
      quantile = sql_quantile("PERCENTILE", window = TRUE),
      median = sql_median("PERCENTILE", window = TRUE)
    )
  )
}

#' @export
sql_table_analyze.Hive <- function(con, table, ...) {
  # https://cwiki.apache.org/confluence/display/Hive/StatsDev
  build_sql(
    "ANALYZE TABLE ", as.sql(table, con = con), " COMPUTE STATISTICS",
    con = con
  )
}

#' @export
last_value_sql.Hive <- function(con, x) {
  translate_sql(last_value(!!x, TRUE), con = con)
}

#' @export
sql_query_set_op.Hive <- function(con, x, y, method, ..., all = FALSE, lvl = 0) {
  # SQLite does not allow parentheses
  method <- paste0(method, if (all) " ALL")
  # `x` and `y` already have the correct indent, so use `build_sql()` instead
  # of `sql_format_clauses()`
  build_sql(
    x, "\n",
    indent_lvl(style_kw(method), lvl = lvl), "\n",
    y,
    con = con
  )
}

#' @export
supports_window_clause.Hive <- function(con) {
  TRUE
}

globalVariables("regexp_replace")
