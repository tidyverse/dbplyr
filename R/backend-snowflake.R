#' Backend: Snowflake
#'
#' @description
#' See `vignette("translate-function")` and `vignette("translate-verb")` for
#' details of overall translation technology.
#'
#' Use `simulate_snowflake()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-snowflake
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_snowflake())
#' lf %>% transmute(x = paste0(z, " times"))
NULL

#' @export
sql_translation.Snowflake <- function(con) {
    sql_variant(
      sql_translator(
        .parent = base_odbc_scalar,
        log10  = function(x) sql_expr(log(10, !!x))
        ),
      base_agg,
      base_win
  )
}

#' @export
#' @rdname backend-snowflake
simulate_snowflake <- function() simulate_dbi("Snowflake")

# There seems to be no concept of ANALYZE TABLE in Snowflake.  I searched for
# functions that performed similar operations, and found none.
# Link to full list: https://docs.snowflake.com/en/sql-reference/sql-all.html
#' @export
sql_table_analyze.Snowflake <- function(con, table, ...) {}
