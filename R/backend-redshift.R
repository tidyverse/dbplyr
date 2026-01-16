#' Redshift backend
#'
#' @description
#' This backend supports Amazon Redshift databases, typically accessed via
#' a `RedshiftConnection` created by [DBI::dbConnect()]. Use `dialect_redshift()`
#' with `lazy_frame()` to see simulated SQL without connecting to a live
#' database.
#'
#' Base translations come from [PostgreSQL backend][dialect_postgres]. There
#' are generally few differences, apart from string manipulation.
#'
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology.
#'
#' @name backend-redshift
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_redshift())
#' lf |> transmute(x = paste(c, " times"))
#' lf |> transmute(x = substr(c, 2, 3))
#' lf |> transmute(x = str_replace_all(c, "a", "z"))
NULL

#' @export
#' @rdname backend-redshift
dialect_redshift <- function() {
  new_sql_dialect(
    "redshift",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}

#' @export
#' @rdname backend-redshift
simulate_redshift <- function() simulate_dbi("RedshiftConnection")

#' @export
sql_dialect.RedshiftConnection <- function(con) {
  dialect_redshift()
}

#' @export
sql_dialect.Redshift <- sql_dialect.RedshiftConnection

#' @export
dbplyr_edition.RedshiftConnection <- function(con) {
  2L
}
#' @export
dbplyr_edition.Redshift <- dbplyr_edition.RedshiftConnection

redshift_round <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  sql_glue("ROUND(({x})::float, {digits})")
}

#' @export
sql_translation.sql_dialect_redshift <- function(con) {
  postgres <- sql_translation(dialect_postgres())

  sql_variant(
    sql_translator(
      .parent = postgres$scalar,

      # https://docs.aws.amazon.com/redshift/latest/dg/r_Numeric_types201.html#r_Numeric_types201-floating-point-types
      as.numeric = sql_cast("FLOAT"),
      as.double = sql_cast("FLOAT"),
      round = redshift_round,

      # https://stackoverflow.com/questions/56708136
      paste = sql_paste_infix(" ", "||"),
      paste0 = sql_paste_infix("", "||"),
      str_c = sql_paste_infix("", "||"),

      str_ilike = function(string, pattern) {
        sql_glue("{string} ILIKE {pattern}")
      },

      # https://docs.aws.amazon.com/redshift/latest/dg/r_SUBSTRING.html
      substr = sql_substr("SUBSTRING"),
      substring = sql_substr("SUBSTRING"),
      str_sub = sql_str_sub("SUBSTRING", "LEN"),

      # https://docs.aws.amazon.com/redshift/latest/dg/REGEXP_REPLACE.html
      str_replace = sql_not_supported("str_replace"),
      str_replace_all = function(string, pattern, replacement) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement})")
      },

      # clock ---------------------------------------------------------------
      add_days = function(x, n, ...) {
        check_dots_empty()
        sql_glue("DATEADD(DAY, {n}, {x})")
      },
      add_years = function(x, n, ...) {
        check_dots_empty()
        sql_glue("DATEADD(YEAR, {n}, {x})")
      },
      date_build = function(year, month = 1L, day = 1L, ..., invalid = NULL) {
        check_unsupported_arg(invalid, allow_null = TRUE)
        sql_glue(
          "TO_DATE(CAST({year} AS TEXT) || '-' || CAST({month} AS TEXT) || '-' || CAST({day} AS TEXT), 'YYYY-MM-DD')"
        )
      },
      get_year = function(x) {
        sql_glue("DATE_PART('year', {x})")
      },
      get_month = function(x) {
        sql_glue("DATE_PART('month', {x})")
      },
      get_day = function(x) {
        sql_glue("DATE_PART('day', {x})")
      },
      date_count_between = function(start, end, precision, ..., n = 1L) {
        check_dots_empty()
        check_unsupported_arg(precision, allowed = "day")
        check_unsupported_arg(n, allowed = 1L)

        sql_glue("DATEDIFF(DAY, {start}, {end})")
      },

      difftime = function(time1, time2, tz, units = "days") {
        check_unsupported_arg(tz)
        check_unsupported_arg(units, allowed = "days")

        sql_glue("DATEDIFF(DAY, {time2}, {time1})")
      }
    ),
    sql_translator(
      .parent = postgres$aggregate,
      # https://docs.aws.amazon.com/redshift/latest/dg/r_LISTAGG.html
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        sql_glue("LISTAGG({x}, {collapse})")
      }
    ),
    sql_translator(
      .parent = postgres$window,
      median = sql_win_not_supported("median", "Redshift"),
      quantile = sql_win_not_supported("quantile", "Redshift"),
      # https://docs.aws.amazon.com/redshift/latest/dg/r_WF_LAG.html
      lag = function(x, n = 1L, order_by = NULL) {
        n <- as.integer(n)
        win_over(
          sql_glue("LAG({x}, {n})"),
          win_current_group(),
          order_by %||% win_current_order(),
          win_current_frame()
        )
      },
      # https://docs.aws.amazon.com/redshift/latest/dg/r_WF_LEAD.html
      lead = function(x, n = 1L, order_by = NULL) {
        n <- as.integer(n)
        win_over(
          sql_glue("LEAD({x}, {n})"),
          win_current_group(),
          order_by %||% win_current_order(),
          win_current_frame()
        )
      },
      # https://docs.aws.amazon.com/redshift/latest/dg/r_LISTAGG.html
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        order <- win_current_order()
        listagg_sql <- sql_glue("LISTAGG({x}, {collapse})")

        if (length(order) > 0) {
          sql <- sql_glue("{listagg_sql} WITHIN GROUP (ORDER BY {order})")
        } else {
          sql <- listagg_sql
        }

        win_over(
          sql,
          partition = win_current_group()
          # Cannot use the order here because LISTAGG requires the ordering in the
          # WITHIN GROUP (ORDER BY ...) clause
        )
      }
    )
  )
}

# https://docs.aws.amazon.com/redshift/latest/dg/r_EXPLAIN.html
#' @export
sql_query_explain.sql_dialect_redshift <- function(con, sql, ...) {
  sql_glue2(con, "EXPLAIN {sql}")
}

#' @export
sql_values_subquery.sql_dialect_redshift <- function(
  con,
  df,
  types,
  lvl = 0,
  ...
) {
  sql_values_subquery_union(con, df, types = types, lvl = lvl)
}
