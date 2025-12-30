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
#' lf |> transmute(x = paste0(d, " times"))
NULL

#' @export
#' @rdname backend-snowflake
dialect_snowflake <- function() {
  new_sql_dialect(
    "snowflake",
    quote_identifier = function(x) sql_quote(x, '"'),
    supports_window_clause = TRUE
  )
}

#' @export
sql_dialect.Snowflake <- function(con) {
  dialect_snowflake()
}

#' @export
dbplyr_edition.Snowflake <- function(con) {
  2L
}

#' @export
sql_translation.sql_dialect_snowflake <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_odbc_scalar,
      log10 = \(x) sql_glue("LOG(10, {x})"),
      grepl = snowflake_grepl,
      round = snowflake_round,
      paste = snowflake_paste(" "),
      paste0 = snowflake_paste(""),
      str_c = function(..., sep = "", collapse = NULL) {
        check_string(sep)
        if (!is.null(collapse)) {
          cli_abort(c(
            "{.arg collapse} not supported in DB translation of {.fn str_c}.",
            i = "Please use {.fn str_flatten} instead."
          ))
        }
        sql_glue("CONCAT_WS({sep}, {...})")
      },
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
      str_locate = function(string, pattern) {
        sql_glue("POSITION({pattern}, {string})")
      },
      str_detect = function(string, pattern, negate = FALSE) {
        con <- sql_current_con()
        check_bool(negate)

        # Snowflake needs backslashes escaped, so we must increase the level of escaping
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        if (negate) {
          sql_glue("REGEXP_INSTR({string}, {pattern}) = 0")
        } else {
          sql_glue("REGEXP_INSTR({string}, {pattern}) != 0")
        }
      },
      str_starts = function(string, pattern, negate = FALSE) {
        con <- sql_current_con()
        check_bool(negate)

        # Snowflake needs backslashes escaped, so we must increase the level of escaping
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        if (negate) {
          sql_glue("REGEXP_INSTR({string}, {pattern}) != 1")
        } else {
          sql_glue("REGEXP_INSTR({string}, {pattern}) = 1")
        }
      },
      str_ends = function(string, pattern, negate = FALSE) {
        con <- sql_current_con()
        check_bool(negate)

        # Snowflake needs backslashes escaped, so we must increase the level of escaping
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        if (negate) {
          sql_glue(
            "REGEXP_INSTR({string}, {pattern}, 1, 1, 1) != (LENGTH({string}) + 1)"
          )
        } else {
          sql_glue(
            "REGEXP_INSTR({string}, {pattern}, 1, 1, 1) = (LENGTH({string}) + 1)"
          )
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
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement}, 1, 1)")
      },
      str_replace_all = function(string, pattern, replacement) {
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement})")
      },
      str_remove = function(string, pattern) {
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        sql_glue("REGEXP_REPLACE({string}, {pattern}, '', 1, 1)")
      },
      str_remove_all = function(string, pattern) {
        pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
        sql_glue("REGEXP_REPLACE({string}, {pattern})")
      },
      str_trim = function(string) {
        sql_glue("TRIM({string})")
      },
      str_squish = function(string) {
        sql_glue("REGEXP_REPLACE(TRIM({string}), '\\\\s+', ' ')")
      },

      # lubridate functions
      # https://docs.snowflake.com/en/sql-reference/functions-date-time.html
      day = function(x) {
        sql_glue("EXTRACT(DAY FROM {x})")
      },
      mday = function(x) {
        sql_glue("EXTRACT(DAY FROM {x})")
      },
      wday = function(x, label = FALSE, abbr = TRUE, week_start = NULL) {
        check_bool(label)
        check_bool(abbr)
        check_number_whole(week_start, allow_null = TRUE)
        if (!label) {
          week_start <- week_start %||% getOption("lubridate.week.start", 7)
          offset <- as.integer(7 - week_start)
          sql_glue("EXTRACT('dayofweek', DATE({x}) + {offset}) + 1")
        } else if (label && !abbr) {
          sql_glue(
            "DECODE(EXTRACT('dayofweek', {x}), 1, 'Monday', 2, 'Tuesday', 3, 'Wednesday', 4, 'Thursday', 5, 'Friday', 6, 'Saturday', 0, 'Sunday')"
          )
        } else if (label && abbr) {
          sql_glue("DAYNAME({x})")
        } else {
          cli::cli_abort("Unrecognized arguments", call = quote(wday()))
        }
      },
      yday = \(x) sql_glue("EXTRACT('dayofyear', {x})"),
      week = function(x) {
        sql_glue("FLOOR((EXTRACT('dayofyear', {x}) - 1) / 7) + 1")
      },
      isoweek = \(x) sql_glue("EXTRACT('weekiso', {x})"),
      month = function(x, label = FALSE, abbr = TRUE) {
        check_bool(label)
        check_bool(abbr)
        if (!label) {
          sql_glue("EXTRACT('month', {x})")
        } else {
          if (abbr) {
            sql_glue("MONTHNAME({x})")
          } else {
            sql_glue(
              "DECODE(EXTRACT('month', {x}), 1, 'January', 2, 'February', 3, 'March', 4, 'April', 5, 'May', 6, 'June', 7, 'July', 8, 'August', 9, 'September', 10, 'October', 11, 'November', 12, 'December')"
            )
          }
        }
      },
      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        check_bool(with_year)
        check_unsupported_arg(fiscal_start, 1)

        if (with_year) {
          sql_glue("(EXTRACT('year', {x}) || '.' || EXTRACT('quarter', {x}))")
        } else {
          sql_glue("EXTRACT('quarter', {x})")
        }
      },
      isoyear = function(x) {
        sql_glue("EXTRACT('year', {x})")
      },
      seconds = function(x) {
        sql_glue("INTERVAL '{x} second'")
      },
      minutes = function(x) {
        sql_glue("INTERVAL '{x} minute'")
      },
      hours = function(x) {
        sql_glue("INTERVAL '{x} hour'")
      },
      days = function(x) {
        sql_glue("INTERVAL '{x} day'")
      },
      weeks = function(x) {
        sql_glue("INTERVAL '{x} week'")
      },
      months = function(x) {
        sql_glue("INTERVAL '{x} month'")
      },
      years = function(x) {
        sql_glue("INTERVAL '{x} year'")
      },
      # https://docs.snowflake.com/en/sql-reference/functions/date_trunc.html
      floor_date = function(x, unit = "seconds") {
        unit <- arg_match(
          unit,
          c(
            "second",
            "minute",
            "hour",
            "day",
            "week",
            "month",
            "quarter",
            "year",
            "seconds",
            "minutes",
            "hours",
            "days",
            "weeks",
            "months",
            "quarters",
            "years"
          )
        )
        sql_glue("DATE_TRUNC({unit}, {x})")
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
        check_dots_empty()
        check_unsupported_arg(invalid, allowed = NULL)
        # https://docs.snowflake.com/en/sql-reference/functions/date_from_parts
        sql_glue("DATE_FROM_PARTS({year}, {month}, {day})")
      },
      get_year = function(x) {
        sql_glue("DATE_PART(YEAR, {x})")
      },
      get_month = function(x) {
        sql_glue("DATE_PART(MONTH, {x})")
      },
      get_day = function(x) {
        sql_glue("DATE_PART(DAY, {x})")
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
      },
      # LEAST / GREATEST on Snowflake will not respect na.rm = TRUE by default (similar to Oracle/Access)
      # https://docs.snowflake.com/en/sql-reference/functions/least
      # https://docs.snowflake.com/en/sql-reference/functions/greatest
      pmin = function(..., na.rm = FALSE) {
        dots <- list(...)
        if (identical(na.rm, TRUE)) {
          snowflake_pmin_pmax_sql_expression(dots = dots, comparison = "<=")
        } else {
          sql_glue("LEAST({dots})")
        }
      },
      pmax = function(..., na.rm = FALSE) {
        dots <- list(...)
        if (identical(na.rm, TRUE)) {
          snowflake_pmin_pmax_sql_expression(dots = dots, comparison = ">=")
        } else {
          sql_glue("GREATEST({dots})")
        }
      },
      `$` = function(x, name) {
        if (is.ident(x)) {
          sql_glue("{x}:{name}")
        } else {
          eval(bquote(`$`(x, .(substitute(name)))))
        }
      }
    ),
    sql_translator(
      .parent = base_agg,
      cor = sql_aggregate_2("CORR"),
      cov = sql_aggregate_2("COVAR_SAMP"),
      all = sql_aggregate("BOOLAND_AGG", "all"),
      any = sql_aggregate("BOOLOR_AGG", "any"),
      sd = sql_aggregate("STDDEV", "sd"),
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        sql_glue("LISTAGG({x}, {collapse})")
      }
    ),
    sql_translator(
      .parent = base_win,
      cor = win_aggregate_2("CORR"),
      cov = win_aggregate_2("COVAR_SAMP"),
      all = win_aggregate("BOOLAND_AGG"),
      any = win_aggregate("BOOLOR_AGG"),
      sd = win_aggregate("STDDEV"),
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        win_over(
          sql_glue("LISTAGG({x}, {collapse})"),
          partition = win_current_group(),
          order = win_current_order()
        )
      },
      row_number = win_rank("ROW_NUMBER", empty_order = TRUE)
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
sql_table_analyze.sql_dialect_snowflake <- function(con, table, ...) {}

snowflake_grepl <- function(
  pattern,
  x,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE
) {
  con <- sql_current_con()

  check_unsupported_arg(perl, FALSE, backend = "Snowflake")
  check_unsupported_arg(fixed, FALSE, backend = "Snowflake")
  check_unsupported_arg(useBytes, FALSE, backend = "Snowflake")

  # https://docs.snowflake.com/en/sql-reference/functions/regexp_instr.html
  # REGEXP_INSTR optional parameters: position, occurrence, option, regex_parameters
  regexp_parameters <- "c"
  if (ignore.case) {
    regexp_parameters <- "i"
  }
  # Snowflake needs backslashes escaped, so we must increase the level of escaping
  pattern <- gsub("\\", "\\\\", pattern, fixed = TRUE)
  sql_glue("REGEXP_INSTR({x}, {pattern}, 1, 1, 0, {regexp_parameters}) != 0")
}

snowflake_round <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  sql_glue("ROUND(({x})::FLOAT, {digits})")
}

# On Snowflake, CONCAT_WS is null if any of its arguments are null.  Paste
# is implemented here to avoid this behavior.
snowflake_paste <- function(default_sep) {
  function(..., sep = default_sep, collapse = NULL) {
    check_collapse(collapse)
    sql_glue("ARRAY_TO_STRING(ARRAY_CONSTRUCT_COMPACT({...}), {sep})")
  }
}

snowflake_pmin_pmax_sql_expression <- function(dots, comparison) {
  dot_combined <- dots[[1]]
  for (i in 2:length(dots)) {
    dot_combined <- snowflake_pmin_pmax_builder(
      dots[i],
      dot_combined,
      comparison
    )
  }
  dot_combined
}

snowflake_pmin_pmax_builder <- function(dot_1, dot_2, comparison) {
  sql_glue(
    "COALESCE(IFF({dot_2} {.sql comparison} {dot_1}, {dot_2}, {dot_1}), {dot_2}, {dot_1})"
  )
}
