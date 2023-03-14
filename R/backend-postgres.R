#' Backend: PostgreSQL
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Many stringr functions
#' * lubridate date-time extraction functions
#' * More standard statistical summaries
#'
#' Use `simulate_postgres()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-postgres
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_postgres())
#' lf %>% summarise(x = sd(b, na.rm = TRUE))
#' lf %>% summarise(y = cor(b, c), z = cov(b, c))
NULL

#' @include verb-copy-to.R
NULL

#' @export
#' @rdname backend-postgres
simulate_postgres <- function() simulate_dbi("PqConnection")

#' @export
dbplyr_edition.PostgreSQL <- function(con) {
  2L
}
#' @export
dbplyr_edition.PqConnection <- dbplyr_edition.PostgreSQL

#' @export
db_connection_describe.PqConnection <- function(con, ...) {
  check_dots_empty0(...)
  info <- dbGetInfo(con)
  host <- if (info$host == "") "localhost" else info$host

  paste0("postgres ", info$serverVersion, " [", info$username, "@",
    host, ":", info$port, "/", info$dbname, "]")
}
#' @export
db_connection_describe.PostgreSQL <- db_connection_describe.PqConnection

postgres_grepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  # https://www.postgresql.org/docs/current/static/functions-matching.html#FUNCTIONS-POSIX-TABLE
  check_unsupported_arg(perl, FALSE, backend = "PostgreSQL")
  check_unsupported_arg(fixed, FALSE, backend = "PostgreSQL")
  check_unsupported_arg(useBytes, FALSE, backend = "PostgreSQL")

  if (ignore.case) {
    sql_expr(((!!x)) %~*% ((!!pattern)))
  } else {
    sql_expr(((!!x)) %~% ((!!pattern)))
  }
}
postgres_round <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  sql_expr(round(((!!x)) %::% numeric, !!digits))
}

#' @export
sql_translation.PqConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      bitwXor = sql_infix("#"),
      log10  = function(x) sql_expr(log(!!x)),
      log    = sql_log(),
      cot    = sql_cot(),
      round  = postgres_round,
      grepl  = postgres_grepl,

      paste  = sql_paste(" "),
      paste0 = sql_paste(""),

      # stringr functions
      # https://www.postgresql.org/docs/9.1/functions-string.html
      # https://www.postgresql.org/docs/9.1/functions-matching.html#FUNCTIONS-POSIX-REGEXP
      str_c = sql_paste(""),

      str_locate  = function(string, pattern) {
        sql_expr(strpos(!!string, !!pattern))
      },
      str_detect = function(string, pattern, negate = FALSE) {
        if (isTRUE(negate)) {
          sql_expr(!(!!string ~ !!pattern))
        } else {
          sql_expr(!!string ~ !!pattern)
        }
      },
      # https://www.postgresql.org/docs/current/functions-matching.html
      str_like = function(string, pattern, ignore_case = TRUE) {
        if (isTRUE(ignore_case)) {
          sql_expr(!!string %ILIKE% !!pattern)
        } else {
          sql_expr(!!string %LIKE% !!pattern)
        }
      },
      str_replace = function(string, pattern, replacement){
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
      },
      str_replace_all = function(string, pattern, replacement){
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement, 'g'))
      },
      str_squish = function(string){
        sql_expr(ltrim(rtrim(regexp_replace(!!string, '\\s+', ' ', 'g'))))
      },
      str_remove = function(string, pattern){
        sql_expr(regexp_replace(!!string, !!pattern, ''))
      },
      str_remove_all = function(string, pattern){
        sql_expr(regexp_replace(!!string, !!pattern, '', 'g'))
      },

      # lubridate functions
      # https://www.postgresql.org/docs/9.1/functions-datetime.html
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
          sql_expr(EXTRACT("dow" %FROM% DATE(!!x) + !!offset) + 1)
        } else if (label && !abbr) {
          sql_expr(TO_CHAR(!!x, "Day"))
        } else if (label && abbr) {
          sql_expr(SUBSTR(TO_CHAR(!!x, "Day"), 1, 3))
        } else {
          cli_abort("Unrecognized arguments to {.arg wday}")
        }
      },
      yday = function(x) sql_expr(EXTRACT(DOY %FROM% !!x)),
      week = function(x) {
        sql_expr(FLOOR ((EXTRACT(DOY %FROM% !!x) - 1L) / 7L) + 1L)
      },
      isoweek = function(x) {
        sql_expr(EXTRACT(WEEK %FROM% !!x))
      },
      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          sql_expr(EXTRACT(MONTH %FROM% !!x))
        } else {
          if (abbr) {
            sql_expr(TO_CHAR(!!x, "Mon"))
          } else {
            sql_expr(TO_CHAR(!!x, "Month"))
          }
        }
      },
      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        check_unsupported_arg(fiscal_start, 1, backend = "PostgreSQL")

        if (with_year) {
          sql_expr((EXTRACT(YEAR %FROM% !!x) || '.' || EXTRACT(QUARTER %FROM% !!x)))
        } else {
          sql_expr(EXTRACT(QUARTER %FROM% !!x))
        }
      },
      isoyear = function(x) {
        sql_expr(EXTRACT(YEAR %FROM% !!x))
      },

      # https://www.postgresql.org/docs/13/datatype-datetime.html#DATATYPE-INTERVAL-INPUT
      seconds = function(x) {
        interval <- paste(x, "seconds")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      minutes = function(x) {
        interval <- paste(x, "minutes")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      hours = function(x) {
        interval <- paste(x, "hours")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      days = function(x) {
        interval <- paste(x, "days")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      weeks = function(x) {
        interval <- paste(x, "weeks")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      months = function(x) {
        interval <- paste(x, "months")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },
      years = function(x) {
        interval <- paste(x, "years")
        sql_expr(CAST(!!interval %AS% INTERVAL))
      },

      # https://www.postgresql.org/docs/current/functions-datetime.html#FUNCTIONS-DATETIME-TRUNC
      floor_date = function(x, unit = "seconds") {
        unit <- arg_match(unit,
          c("second", "minute", "hour", "day", "week", "month", "quarter", "year")
        )
        sql_expr(DATE_TRUNC(!!unit, !!x))
      },
    ),
    sql_translator(.parent = base_agg,
      cor = sql_aggregate_2("CORR"),
      cov = sql_aggregate_2("COVAR_SAMP"),
      sd = sql_aggregate("STDDEV_SAMP", "sd"),
      var = sql_aggregate("VAR_SAMP", "var"),
      all = sql_aggregate("BOOL_AND", "all"),
      any = sql_aggregate("BOOL_OR", "any"),
      str_flatten = function(x, collapse = "") {
        sql_expr(string_agg(!!x, !!collapse))
      }
    ),
    sql_translator(.parent = base_win,
      cor = win_aggregate_2("CORR"),
      cov = win_aggregate_2("COVAR_SAMP"),
      sd =  win_aggregate("STDDEV_SAMP"),
      var = win_aggregate("VAR_SAMP"),
      all = win_aggregate("BOOL_AND"),
      any = win_aggregate("BOOL_OR"),
      str_flatten = function(x, collapse = "") {
        win_over(
          sql_expr(string_agg(!!x, !!collapse)),
          partition = win_current_group(),
          order = win_current_order()
        )
      },
      median = sql_win_not_supported("median", "PostgreSQL"),
      quantile = sql_win_not_supported("quantile", "PostgreSQL")
    )
  )
}
#' @export
sql_translation.PostgreSQL <- sql_translation.PqConnection

#' @export
sql_expr_matches.PqConnection <- function(con, x, y, ...) {
  check_dots_empty0(...)

  # https://www.postgresql.org/docs/current/functions-comparison.html
  build_sql(x, " IS NOT DISTINCT FROM ", y, con = con)
}
#' @export
sql_expr_matches.PostgreSQL <- sql_expr_matches.PqConnection

# http://www.postgresql.org/docs/9.3/static/sql-explain.html
#' @export
sql_query_explain.PqConnection <- function(con, sql, format = "text", ...) {
  format <- match.arg(format, c("text", "json", "yaml", "xml"))

  build_sql(
    "EXPLAIN ",
    if (!is.null(format)) sql(paste0("(FORMAT ", format, ") ")),
    sql,
    con = con
  )
}
#' @export
sql_query_explain.PostgreSQL <- sql_query_explain.PqConnection

#' @export
sql_query_insert.PqConnection <- function(con,
                                          x_name,
                                          y,
                                          by,
                                          conflict = c("error", "ignore"),
                                          ...,
                                          returning_cols = NULL,
                                          method = NULL) {
  method <- method %||% "on_conflict"
  arg_match(method, c("on_conflict", "where_not_exists"), error_arg = "method")
  if (method == "where_not_exists") {
    return(NextMethod("sql_query_insert"))
  }

  # https://stackoverflow.com/questions/17267417/how-to-upsert-merge-insert-on-duplicate-update-in-postgresql
  # https://www.sqlite.org/lang_UPSERT.html
  conflict <- rows_check_conflict(conflict)

  parts <- rows_insert_prep(con, x_name, y, by, lvl = 0)
  by_sql <- escape(ident(by), parens = TRUE, collapse = ", ", con = con)

  clauses <- list(
    parts$insert_clause,
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    sql_clause("ON CONFLICT", by_sql),
    {if (conflict == "ignore") sql("DO NOTHING")},
    sql_returning_cols(con, returning_cols, x_name)
  )
  sql_format_clauses(clauses, lvl = 0, con)
}
#' @export
sql_query_insert.PostgreSQL <- sql_query_insert.PqConnection

#' @export
sql_query_upsert.PqConnection <- function(con,
                                          x_name,
                                          y,
                                          by,
                                          update_cols,
                                          ...,
                                          returning_cols = NULL,
                                          method = NULL) {
  method <- method %||% "on_conflict"
  arg_match(method, c("cte_update", "on_conflict"), error_arg = "method")

  if (method == "cte_update") {
    return(NextMethod("sql_query_upsert"))
  }

  # https://stackoverflow.com/questions/17267417/how-to-upsert-merge-insert-on-duplicate-update-in-postgresql
  # https://www.sqlite.org/lang_UPSERT.html
  parts <- rows_prep(con, x_name, y, by, lvl = 0)

  update_values <- set_names(
    sql_table_prefix(con, update_cols, ident("excluded")),
    update_cols
  )
  update_cols <- sql_escape_ident(con, update_cols)

  insert_cols <- escape(ident(colnames(y)), collapse = ", ", parens = TRUE, con = con)
  by_sql <- escape(ident(by), parens = TRUE, collapse = ", ", con = con)
  clauses <- list(
    sql_clause_insert(con, insert_cols, x_name),
    sql_clause_select(con, sql("*")),
    sql_clause_from(parts$from),
    # `WHERE true` is required for SQLite
    sql("WHERE true"),
    sql_clause("ON CONFLICT ", by_sql),
    sql("DO UPDATE"),
    sql_clause_set(update_cols, update_values),
    sql_returning_cols(con, returning_cols, x_name)
  )
  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
sql_query_upsert.PostgreSQL <- sql_query_upsert.PqConnection

#' @export
sql_values_subquery.PqConnection <- sql_values_subquery_column_alias

#' @export
sql_values_subquery.PostgreSQL <- sql_values_subquery.PqConnection

#' @export
sql_escape_date.PostgreSQL <- function(con, x) {
  DBI::dbQuoteLiteral(con, x)
}
#' @export
sql_escape_date.PqConnection <- sql_escape_date.PostgreSQL


#' @export
supports_window_clause.PqConnection <- function(con) {
  TRUE
}

#' @export
supports_window_clause.PostgreSQL <- function(con) {
  TRUE
}

globalVariables(c("strpos", "%::%", "%FROM%", "%ILIKE%", "DATE", "EXTRACT", "TO_CHAR", "string_agg", "%~*%", "%~%", "MONTH", "DOY", "DATE_TRUNC", "INTERVAL", "FLOOR", "WEEK"))
