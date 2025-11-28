#' Backend: Redshift
#'
#' @description
#' Base translations come from [PostgreSQL backend][simulate_postgres]. There
#' are generally few differences, apart from string manipulation.
#'
#' Use `simulate_redshift()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-redshift
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_redshift())
#' lf |> transmute(x = paste(c, " times"))
#' lf |> transmute(x = substr(c, 2, 3))
#' lf |> transmute(x = str_replace_all(c, "a", "z"))
NULL

#' @export
#' @rdname backend-redshift
simulate_redshift <- function() simulate_dbi("RedshiftConnection")

#' @export
dbplyr_edition.RedshiftConnection <- function(con) {
  2L
}
#' @export
dbplyr_edition.Redshift <- dbplyr_edition.RedshiftConnection

redshift_round <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  sql_expr(round(((!!x)) %::% float, !!digits))
}

#' @export
sql_translation.RedshiftConnection <- function(con) {
  postgres <- sql_translation.PostgreSQL(con)

  sql_variant(
    sql_translator(
      .parent = postgres$scalar,

      # https://docs.aws.amazon.com/redshift/latest/dg/r_Numeric_types201.html#r_Numeric_types201-floating-point-types
      as.numeric = sql_cast("FLOAT"),
      as.double = sql_cast("FLOAT"),
      round = redshift_round,

      # https://stackoverflow.com/questions/56708136
      paste = sql_paste_redshift(" "),
      paste0 = sql_paste_redshift(""),
      str_c = sql_paste_redshift(""),

      # https://docs.aws.amazon.com/redshift/latest/dg/r_SUBSTRING.html
      substr = sql_substr("SUBSTRING"),
      substring = sql_substr("SUBSTRING"),
      str_sub = sql_str_sub("SUBSTRING", "LEN"),

      # https://docs.aws.amazon.com/redshift/latest/dg/REGEXP_REPLACE.html
      str_replace = sql_not_supported("str_replace"),
      str_replace_all = function(string, pattern, replacement) {
        sql_expr(REGEXP_REPLACE(!!string, !!pattern, !!replacement))
      },

      # clock ---------------------------------------------------------------
      add_days = function(x, n, ...) {
        check_dots_empty()
        sql_expr(DATEADD(DAY, !!n, !!x))
      },
      add_years = function(x, n, ...) {
        check_dots_empty()
        sql_expr(DATEADD(YEAR, !!n, !!x))
      },
      date_build = function(year, month = 1L, day = 1L, ..., invalid = NULL) {
        check_unsupported_arg(invalid, allow_null = TRUE)
        glue_sql2(
          sql_current_con(),
          "TO_DATE(CAST({.val year} AS TEXT) || '-' CAST({.val month} AS TEXT) || '-' || CAST({.val day} AS TEXT)), 'YYYY-MM-DD')"
        )
      },
      get_year = function(x) {
        sql_expr(DATE_PART('year', !!x))
      },
      get_month = function(x) {
        sql_expr(DATE_PART('month', !!x))
      },
      get_day = function(x) {
        sql_expr(DATE_PART('day', !!x))
      },
      date_count_between = function(start, end, precision, ..., n = 1L) {
        check_dots_empty()
        check_unsupported_arg(precision, allowed = "day")
        check_unsupported_arg(n, allowed = 1L)

        sql_expr(DATEDIFF(DAY, !!start, !!end))
      },

      difftime = function(time1, time2, tz, units = "days") {
        check_unsupported_arg(tz)
        check_unsupported_arg(units, allowed = "days")

        sql_expr(DATEDIFF(DAY, !!time2, !!time1))
      }
    ),
    sql_translator(
      .parent = postgres$aggregate,
      # https://docs.aws.amazon.com/redshift/latest/dg/r_LISTAGG.html
      str_flatten = function(x, collapse = "") {
        sql_expr(LISTAGG(!!x, !!collapse))
      }
    ),
    sql_translator(
      .parent = postgres$window,
      # https://docs.aws.amazon.com/redshift/latest/dg/r_WF_LAG.html
      lag = function(x, n = 1L, order_by = NULL) {
        win_over(
          sql_expr(LAG(!!x, !!as.integer(n))),
          win_current_group(),
          order_by %||% win_current_order(),
          win_current_frame()
        )
      },
      # https://docs.aws.amazon.com/redshift/latest/dg/r_WF_LEAD.html
      lead = function(x, n = 1L, order_by = NULL) {
        win_over(
          sql_expr(LEAD(!!x, !!as.integer(n))),
          win_current_group(),
          order_by %||% win_current_order(),
          win_current_frame()
        )
      },
      # https://docs.aws.amazon.com/redshift/latest/dg/r_LISTAGG.html
      str_flatten = function(x, collapse = "") {
        order <- win_current_order()
        listagg_sql <- sql_expr(LISTAGG(!!x, !!collapse))

        if (length(order) > 0) {
          sql <- glue_sql2(
            sql_current_con(),
            "{listagg_sql} WITHIN GROUP (ORDER BY {order})"
          )
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

#' @export
sql_translation.Redshift <- sql_translation.RedshiftConnection

sql_paste_redshift <- function(sep) {
  sql_paste_infix(sep, "||", function(x) sql_expr(cast(!!x %as% text)))
}

# https://docs.aws.amazon.com/redshift/latest/dg/r_EXPLAIN.html
#' @export
sql_query_explain.Redshift <- function(con, sql, ...) {
  glue_sql2(con, "EXPLAIN {sql}")
}

#' @export
sql_query_explain.RedshiftConnection <- sql_query_explain.Redshift

#' @export
sql_values_subquery.Redshift <- function(con, df, types, lvl = 0, ...) {
  sql_values_subquery_union(con, df, types = types, lvl = lvl)
}

#' @export
sql_values_subquery.RedshiftConnection <- sql_values_subquery.Redshift

#' @export
supports_window_clause.Redshift <- function(con) {
  FALSE
}

#' @export
supports_window_clause.RedshiftConnection <- supports_window_clause.Redshift

utils::globalVariables(c(
  "REGEXP_REPLACE",
  "LAG",
  "LEAD",
  "LISTAGG",
  "float",
  "text",
  "DATE_PART"
))
