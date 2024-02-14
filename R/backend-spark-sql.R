#' Backend: Databricks Spark SQL
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are better translation of statistical aggregate functions
#' (e.g. `var()`, `median()`) and use of temporary views instead of temporary
#' tables when copying data.
#'
#' Use `simulate_spark_sql()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-spark-sql
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, d = 2, c = "z", con = simulate_spark_sql())
#'
#' lf %>% summarise(x = median(d, na.rm = TRUE))
#' lf %>% summarise(x = var(c, na.rm = TRUE), .by = d)
#'
#' lf %>% mutate(x = first(c))
#' lf %>% mutate(x = first(c), .by = d)
NULL

#' @export
#' @rdname backend-spark-sql
simulate_spark_sql <- function() simulate_dbi("Spark SQL")

#' @export
`dbplyr_edition.Spark SQL` <- function(con) {
  2L
}

#' @export
`sql_translation.Spark SQL` <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
       # clock ---------------------------------------------------------------
       add_days = function(x, n, ...) {
         check_dots_empty()
         sql_expr(date_add(!!x, !!n))
       },
       add_years = function(x, n, ...) {
         check_dots_empty()
         sql_expr(add_months(!!!x, !!n*12))
       },
       date_build = function(year, month = 1L, day = 1L, ..., invalid = NULL) {
         sql_expr(make_date(!!year, !!month, !!day))
       },
       get_year = function(x) {
         sql_expr(date_part('YEAR', !!x))
       },
       get_month = function(x) {
         sql_expr(date_part('MONTH', !!x))
       },
       get_day = function(x) {
         sql_expr(date_part('DAY', !!x))
       },

       difftime = function(time1, time2, tz, units = "days") {

         if (!missing(tz)) {
           cli::cli_abort("The {.arg tz} argument is not supported for SQL backends.")
         }

         if (units[1] != "days") {
           cli::cli_abort('The only supported value for {.arg units} on SQL backends is "days"')
         }

         sql_expr(datediff(!!time2, !!time1))
       }
    ),
    sql_translator(.parent = base_odbc_agg,
      var = sql_aggregate("VARIANCE", "var"),
      quantile = sql_quantile("PERCENTILE"),
      median = sql_aggregate("MEDIAN"),
      first = function(x, na_rm = FALSE) {
        check_na_rm(na_rm)
        glue_sql2(sql_current_con(), "FIRST({.val x})")
      },
      last = function(x, na_rm = FALSE) {
        check_na_rm(na_rm)
        glue_sql2(sql_current_con(), "LAST({.val x})")
      },
    ),
    sql_translator(.parent = base_odbc_win,
      var = win_aggregate("VARIANCE"),
      quantile = sql_quantile("PERCENTILE", window = TRUE),
      median = win_aggregate("MEDIAN"),
      first = function(x, order_by = NULL, na_rm = FALSE) {
        sql_nth(x, 1L, order_by = order_by, na_rm = na_rm, ignore_nulls = "bool")
      },
      last = function(x, order_by = NULL, na_rm = FALSE) {
        sql_nth(x, Inf, order_by = order_by, na_rm = na_rm, ignore_nulls = "bool")
      },
      nth = function(x, n, order_by = NULL, na_rm = FALSE) {
        sql_nth(x, n, order_by = order_by, na_rm = na_rm, ignore_nulls = "bool")
      },
    )
  )
}

#' @export
`sql_table_analyze.Spark SQL` <- function(con, table, ...) {
  # https://docs.databricks.com/en/sql/language-manual/sql-ref-syntax-aux-analyze-table.html
  glue_sql2(con, "ANALYZE TABLE {.tbl table} COMPUTE STATISTICS")
}

#' @export
`supports_window_clause.Spark SQL` <- function(con) {
  TRUE
}

#' @export
`db_copy_to.Spark SQL` <- function(con,
                                   table,
                                   values,
                                   ...,
                                   overwrite = FALSE,
                                   types = NULL,
                                   temporary = TRUE,
                                   unique_indexes = NULL,
                                   indexes = NULL,
                                   analyze = TRUE,
                                   in_transaction = FALSE) {

  if (temporary) {
    sql <- sql_values_subquery(con, values, types = types, lvl = 1)
    db_compute(con, table, sql, overwrite = overwrite)
  } else {
    NextMethod()
  }
}

#' @export
`db_compute.Spark SQL` <- function(con,
                                   table,
                                   sql,
                                   ...,
                                   overwrite = FALSE,
                                   temporary = TRUE,
                                   unique_indexes = list(),
                                   indexes = list(),
                                   analyze = TRUE,
                                   in_transaction = FALSE) {

  if (!temporary) {
    cli::cli_abort("Spark SQL only support temporary tables")
  }

  table <- as_table_ident(table)
  sql <- glue_sql2(
    con,
    "CREATE ", if (overwrite) "OR REPLACE ",
    "TEMPORARY VIEW {.tbl {table}} AS \n",
    "{.from {sql}}"
  )
  DBI::dbExecute(con, sql)

  table
}

utils::globalVariables("regexp_replace")
