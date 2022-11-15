#' Backend: Snowflake
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
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
#' lf %>% transmute(x = paste0(d, " times"))
NULL

#' @export
sql_translation.Snowflake <- function(con, ...) {
  sql_variant(
    sql_translator(
      .parent = base_odbc_scalar,
      log10 = function(x) sql_expr(log(10, !!x)),
      grepl = snowflake_grepl,
      round = snowflake_round,
      paste = snowflake_paste(" "),
      paste0 = snowflake_paste(""),
      str_c = function(..., sep = "", collapse = NULL) {
        if (!is.null(collapse)) {
          cli_abort(c(
            "{.arg collapse} not supported in DB translation of {.fn str_c}.",
            i = "Please use {.fn str_flatten} instead."
          ))
        }
        sql_call2("CONCAT_WS", sep, ...)
      },
      str_locate = function(string, pattern) {
        sql_expr(POSITION(!!pattern, !!string))
      },
      # REGEXP on Snowflaake "implicitly anchors a pattern at both ends", which
      # str_detect does not.  Left- and right-pad `pattern` with .* to get
      # str_detect-like behavior
      str_detect = function(string, pattern, negate = FALSE) {
        if (isTRUE(negate)) {
          sql_expr(!(((!!string)) %REGEXP% (".*" || (!!pattern) || ".*")))
        } else {
          sql_expr(((!!string)) %REGEXP% (".*" || (!!pattern) || ".*"))
        }
      },
      # On Snowflake, REGEXP_REPLACE is used like this:
      # REGEXP_REPLACE( <subject> , <pattern> [ , <replacement> ,
      #                <position> , <occurrence> , <parameters> ] )
      # so we must set <occurrence> to 1 if not replacing all.  See:
      # https://docs.snowflake.com/en/sql-reference/functions/regexp_replace.html
      # Also, Snowflake needs backslashes escaped, so we must increase the
      # level of escaping by 1
      str_replace = function(string, pattern, replacement) {
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement, 1, 1))
      },
      str_replace_all = function(string, pattern, replacement) {
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
      },
      str_remove = function(string, pattern) {
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        sql_expr(regexp_replace(!!string, !!pattern, "", 1, 1))
      },
      str_remove_all = function(string, pattern) {
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        sql_expr(regexp_replace(!!string, !!pattern))
      },
      str_trim = function(string) {
        sql_expr(trim(!!string))
      },
      str_squish = function(string) {
        sql_expr(regexp_replace(trim(!!string), "\\\\s+", " "))
      },


      # lubridate functions
      # https://docs.snowflake.com/en/sql-reference/functions-date-time.html
      day = function(x) {
        sql_expr(EXTRACT(DAY %FROM% !!x))
      },
      mday = function(x) {
        sql_expr(EXTRACT(DAY %FROM% !!x))
      },
      wday = function(x, label = FALSE, abbr = TRUE, week_start = NULL) {
        if (!label) {
          week_start <- week_start %||% getOption("lubridate.week.start", 7)
          offset <- as.integer(7 - week_start)
          sql_expr(EXTRACT("dayofweek", DATE(!!x) + !!offset) + 1)
        } else if (label && !abbr) {
          sql_expr(
            DECODE(
              EXTRACT("dayofweek", !!x),
              1, "Monday",
              2, "Tuesday",
              3, "Wednesday",
              4, "Thursday",
              5, "Friday",
              6, "Saturday",
              0, "Sunday"
            )
          )
        } else if (label && abbr) {
          sql_expr(DAYNAME(!!x))
        } else {
          abort("Unrecognized arguments to `wday`")
        }
      },
      yday = function(x) sql_expr(EXTRACT("dayofyear", !!x)),
      week = function(x) {
        sql_expr(FLOOR((EXTRACT("dayofyear", !!x) - 1L) / 7L) + 1L)
      },
      isoweek = function(x) sql_expr(EXTRACT("weekiso", !!x)),
      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          sql_expr(EXTRACT("month", !!x))
        } else {
          if (abbr) {
            sql_expr(MONTHNAME(!!x))
          } else {
            sql_expr(
              DECODE(
                EXTRACT("month", !!x),
                1, "January",
                2, "February",
                3, "March",
                4, "April",
                5, "May",
                6, "June",
                7, "July",
                8, "August",
                9, "September",
                10, "October",
                11, "November",
                12, "December"
              )
            )
          }
        }
      },
      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        if (fiscal_start != 1) {
          abort("`fiscal_start` is not supported in Snowflake translation. Must be 1.")
        }

        if (with_year) {
          sql_expr((EXTRACT("year", !!x) || "." || EXTRACT("quarter", !!x)))
        } else {
          sql_expr(EXTRACT("quarter", !!x))
        }
      },
      isoyear = function(x) {
        sql_expr(EXTRACT("year", !!x))
      },
      seconds = function(x) {
        build_sql("INTERVAL '", x, " second'")
      },
      minutes = function(x) {
        build_sql("INTERVAL '", x, " minute'")
      },
      hours = function(x) {
        build_sql("INTERVAL '", x, " hour'")
      },
      days = function(x) {
        build_sql("INTERVAL '", x, " day'")
      },
      weeks = function(x) {
        build_sql("INTERVAL '", x, " week'")
      },
      months = function(x) {
        build_sql("INTERVAL '", x, " month'")
      },
      years = function(x) {
        build_sql("INTERVAL '", x, " year'")
      },
      # https://docs.snowflake.com/en/sql-reference/functions/date_trunc.html
      floor_date = function(x, unit = "seconds") {
        unit <- arg_match(unit,
          c("second", "minute", "hour", "day", "week", "month", "quarter", "year",
            "seconds", "minutes", "hours", "days", "weeks", "months", "quarters", "years")
        )
        sql_expr(DATE_TRUNC(!!unit, !!x))
      }
    ),
    sql_translator(
      .parent = base_agg,
      cor = sql_aggregate_2("CORR"),
      cov = sql_aggregate_2("COVAR_SAMP"),
      all = sql_aggregate("BOOLAND_AGG", "all"),
      any = sql_aggregate("BOOLOR_AGG", "any"),
      sd = sql_aggregate("STDDEV", "sd"),
      str_flatten = function(x, collapse = "") {
        sql_expr(LISTAGG(!!x, !!collapse))
      }
    ),
    sql_translator(
      .parent = base_win,
      cor = win_aggregate_2("CORR"),
      cov = win_aggregate_2("COVAR_SAMP"),
      all = win_aggregate("BOOLAND_AGG"),
      any = win_aggregate("BOOLOR_AGG"),
      sd = win_aggregate("STDDEV"),
      str_flatten = function(x, collapse = "") {
        win_over(
          sql_expr(LISTAGG(!!x, !!collapse)),
          partition = win_current_group(),
          order = win_current_order()
        )
      }
    )
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

snowflake_grepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  # https://docs.snowflake.com/en/sql-reference/functions/regexp.html
  if (perl || fixed || useBytes || ignore.case) {
    cli_abort("{.arg {c('perl', 'fixed', 'useBytes', 'ignore.case')}} parameters are unsupported.")
  }
  # REGEXP on Snowflaake "implicitly anchors a pattern at both ends", which
  # grepl does not.  Left- and right-pad `pattern` with .* to get grepl-like
  # behavior
  sql_expr(((!!x)) %REGEXP% (".*" || !!paste0('(', pattern, ')') || ".*"))
}
snowflake_round <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  sql_expr(round(((!!x)) %::% FLOAT, !!digits))
}

# On Snowflake, CONCAT_WS is null if any of its arguments are null.  Paste
# is implemented here to avoid this behavior.
snowflake_paste <- function(default_sep) {
  function(..., sep = default_sep, collapse = NULL) {
    check_collapse(collapse)
    sql_call2(
      "ARRAY_TO_STRING",
      sql_call2("ARRAY_CONSTRUCT_COMPACT", ...), sep
    )
  }
}

utils::globalVariables(c("%REGEXP%", "DAYNAME", "DECODE", "FLOAT", "MONTHNAME", "POSITION", "trim"))
