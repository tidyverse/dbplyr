#' Backend: Hive
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are a scattering of custom translations provided by users.
#'
#' Use `simulate_hive()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-hive
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, d = 2, c = "z", con = simulate_hive())
#' lf %>% transmute(x = cot(b))
#' lf %>% transmute(x = bitwShiftL(c, 1L))
#' lf %>% transmute(x = str_replace_all(z, "a", "b"))
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
    "ANALYZE TABLE ", as.sql(table), " COMPUTE STATISTICS",
    con = con
  )
}

globalVariables("regexp_replace")
