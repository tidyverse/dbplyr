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
#' lf |> summarise(x = median(d, na.rm = TRUE))
#' lf |> summarise(x = var(c, na.rm = TRUE), .by = d)
#'
#' lf |> mutate(x = first(c))
#' lf |> mutate(x = first(c), .by = d)
NULL

#' @export
#' @rdname backend-spark-sql
simulate_spark_sql <- function() simulate_dbi("Spark SQL")

dialect_spark_sql <- function() {
  new_sql_dialect(
    "spark_sql",
    quote_identifier = function(x) sql_quote(x, '"'),
    supports_window_clause = TRUE
  )
}

#' @export
`sql_dialect.Spark SQL` <- function(con) {
  dialect_spark_sql()
}

#' @export
`dbplyr_edition.Spark SQL` <- function(con) {
  2L
}

#' @export
sql_translation.sql_dialect_spark_sql <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_odbc_scalar,
      # stringr ---------------------------------------------------------------
      str_like = function(string, pattern, ignore_case = deprecated()) {
        ignore_case <- deprecate_ignore_case(ignore_case)
        if (ignore_case) {
          sql_glue("{string} ILIKE {pattern}")
        } else {
          sql_glue("{string} LIKE {pattern}")
        }
      },
      str_ilike = function(string, pattern) {
        sql_glue("{string} ILIKE {pattern}")
      },
      # clock ---------------------------------------------------------------
      add_days = function(x, n, ...) {
        check_dots_empty()
        sql_glue("DATE_ADD({x}, {n})")
      },
      add_years = function(x, n, ...) {
        check_dots_empty()
        sql_glue("ADD_MONTHS({x}, {n} * 12)")
      },
      date_build = function(year, month = 1L, day = 1L, ..., invalid = NULL) {
        check_unsupported_arg(invalid, allow_null = TRUE)
        sql_glue("MAKE_DATE({year}, {month}, {day})")
      },
      get_year = function(x) {
        sql_glue("DATE_PART('YEAR', {x})")
      },
      get_month = function(x) {
        sql_glue("DATE_PART('MONTH', {x})")
      },
      get_day = function(x) {
        sql_glue("DATE_PART('DAY', {x})")
      },
      date_count_between = function(start, end, precision, ..., n = 1L) {
        check_dots_empty()
        check_unsupported_arg(precision, allowed = "day")
        check_unsupported_arg(n, allowed = 1L)

        sql_glue("DATEDIFF({end}, {start})")
      },

      difftime = function(time1, time2, tz, units = "days") {
        check_unsupported_arg(tz)
        check_unsupported_arg(units, allowed = "days")

        sql_glue("DATEDIFF({time2}, {time1})")
      }
    ),
    sql_translator(
      .parent = base_odbc_agg,
      var = sql_aggregate("VARIANCE", "var"),
      quantile = sql_quantile("PERCENTILE"),
      median = sql_aggregate("MEDIAN"),
      first = function(x, na_rm = FALSE) {
        sql_check_na_rm(na_rm)
        sql_glue("FIRST({x})")
      },
      last = function(x, na_rm = FALSE) {
        sql_check_na_rm(na_rm)
        sql_glue("LAST({x})")
      },
    ),
    sql_translator(
      .parent = base_odbc_win,
      var = win_aggregate("VARIANCE"),
      quantile = sql_quantile("PERCENTILE", window = TRUE),
      median = win_aggregate("MEDIAN"),
      first = function(x, order_by = NULL, na_rm = FALSE) {
        sql_nth(
          x,
          1L,
          order_by = order_by,
          na_rm = na_rm,
          ignore_nulls = "bool"
        )
      },
      last = function(x, order_by = NULL, na_rm = FALSE) {
        sql_nth(
          x,
          Inf,
          order_by = order_by,
          na_rm = na_rm,
          ignore_nulls = "bool"
        )
      },
      nth = function(x, n, order_by = NULL, na_rm = FALSE) {
        sql_nth(x, n, order_by = order_by, na_rm = na_rm, ignore_nulls = "bool")
      },
    )
  )
}

#' @export
sql_table_analyze.sql_dialect_spark_sql <- function(con, table, ...) {
  # https://docs.databricks.com/en/sql/language-manual/sql-ref-syntax-aux-analyze-table.html
  sql_glue2(con, "ANALYZE TABLE {.tbl table} COMPUTE STATISTICS")
}

#' @export
`db_copy_to.Spark SQL` <- function(
  con,
  table,
  values,
  ...,
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  in_transaction = FALSE
) {
  sql <- sql_values_subquery(con, values, types = types, lvl = 1)
  db_compute(con, table, sql, overwrite = overwrite, temporary = temporary)
}

#' @export
`db_compute.Spark SQL` <- function(
  con,
  table,
  sql,
  ...,
  overwrite = FALSE,
  temporary = TRUE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  in_transaction = FALSE
) {
  check_bool(overwrite)
  check_bool(temporary)

  action <- if (overwrite) "CREATE OR REPLACE" else "CREATE"
  type <- if (temporary) "TEMPORARY VIEW" else "TABLE"

  sql <- as_table_source(sql)
  sql <- sql_glue2(con, "{.sql action} {.sql type} {.tbl table} AS \n{sql}")
  DBI::dbExecute(con, sql)

  table
}
