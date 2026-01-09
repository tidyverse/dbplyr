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
#' lf |> summarise(x = sd(b, na.rm = TRUE))
#' lf |> summarise(y = cor(b, c), z = cov(b, c))
NULL

#' @export
#' @rdname backend-postgres
simulate_postgres <- function() simulate_dbi("PqConnection")

dialect_postgres <- function() {
  new_sql_dialect(
    "postgres",
    quote_identifier = function(x) sql_quote(x, '"'),
    has_window_clause = TRUE
  )
}

#' @export
sql_dialect.PqConnection <- function(con) {
  dialect_postgres()
}

#' @export
sql_dialect.PostgreSQL <- function(con) {
  dialect_postgres()
}

#' @export
dbplyr_edition.PostgreSQL <- function(con) {
  2L
}
#' @export
dbplyr_edition.PqConnection <- dbplyr_edition.PostgreSQL

#' @export
db_connection_describe.PqConnection <- function(con, ...) {
  info <- DBI::dbGetInfo(con)
  host <- if (info$host == "") "localhost" else info$host

  paste0(
    "postgres ",
    info$serverVersion,
    " [",
    info$username,
    "@",
    host,
    ":",
    info$port,
    "/",
    info$dbname,
    "]"
  )
}
#' @export
db_connection_describe.PostgreSQL <- db_connection_describe.PqConnection

postgres_grepl <- function(
  pattern,
  x,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE
) {
  # https://www.postgresql.org/docs/current/static/functions-matching.html#FUNCTIONS-POSIX-TABLE
  check_unsupported_arg(perl, FALSE, backend = "PostgreSQL")
  check_unsupported_arg(fixed, FALSE, backend = "PostgreSQL")
  check_unsupported_arg(useBytes, FALSE, backend = "PostgreSQL")
  check_bool(ignore.case)

  if (ignore.case) {
    sql_glue("({x}) ~* ({pattern})")
  } else {
    sql_glue("({x}) ~ ({pattern})")
  }
}
postgres_round <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  sql_glue("ROUND(({x})::numeric, {digits})")
}

# https://neon.com/postgresql/postgresql-date-functions/postgresql-make_interval
postgres_period <- function(x, unit) {
  sql_glue("MAKE_INTERVAL({.sql unit} => {x})")
}

#' @export
sql_translation.sql_dialect_postgres <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_scalar,
      bitwXor = sql_infix("#"),
      log10 = \(x) sql_glue("LOG({x})"),
      log = sql_log(),
      cot = sql_cot(),
      round = postgres_round,
      grepl = postgres_grepl,

      paste = sql_paste(" "),
      paste0 = sql_paste(""),

      # stringr functions
      # https://www.postgresql.org/docs/9.1/functions-string.html
      # https://www.postgresql.org/docs/9.1/functions-matching.html#FUNCTIONS-POSIX-REGEXP
      str_c = sql_paste(""),

      str_locate = function(string, pattern) {
        sql_glue("STRPOS({string}, {pattern})")
      },
      # https://www.postgresql.org/docs/9.1/functions-string.html
      str_detect = function(string, pattern, negate = FALSE) {
        sql_str_pattern_switch(
          string = string,
          pattern = {{ pattern }},
          negate = negate,
          f_fixed = sql_str_detect_fixed_position("detect"),
          f_regex = function(string, pattern, negate = FALSE) {
            if (isTRUE(negate)) {
              sql_glue("!({string} ~ {pattern})")
            } else {
              sql_glue("{string} ~ {pattern}")
            }
          }
        )
      },
      str_ends = function(string, pattern, negate = FALSE) {
        sql_str_pattern_switch(
          string = string,
          pattern = {{ pattern }},
          negate = negate,
          f_fixed = sql_str_detect_fixed_position("end")
        )
      },
      # https://www.postgresql.org/docs/current/functions-matching.html
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
      str_replace = function(string, pattern, replacement) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement})")
      },
      str_replace_all = function(string, pattern, replacement) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement}, 'g')")
      },
      str_squish = function(string) {
        sql_glue("LTRIM(RTRIM(REGEXP_REPLACE({string}, '\\s+', ' ', 'g')))")
      },
      str_remove = function(string, pattern) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, '')")
      },
      str_remove_all = function(string, pattern) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, '', 'g')")
      },
      str_starts = function(string, pattern, negate = FALSE) {
        sql_str_pattern_switch(
          string = string,
          pattern = {{ pattern }},
          negate = negate,
          f_fixed = sql_str_detect_fixed_position("start")
        )
      },

      # lubridate functions
      # https://www.postgresql.org/docs/9.1/functions-datetime.html
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
          sql_glue("EXTRACT('dow' FROM DATE({x}) + {offset}) + 1")
        } else if (label && !abbr) {
          sql_glue("TO_CHAR({x}, 'Day')")
        } else if (label && abbr) {
          sql_glue("SUBSTR(TO_CHAR({x}, 'Day'), 1, 3)")
        } else {
          cli_abort("Unrecognized arguments to {.arg wday}")
        }
      },
      yday = \(x) sql_glue("EXTRACT(DOY FROM {x})"),
      week = function(x) {
        sql_glue("FLOOR((EXTRACT(DOY FROM {x}) - 1) / 7) + 1")
      },
      isoweek = function(x) {
        sql_glue("EXTRACT(WEEK FROM {x})")
      },
      month = function(x, label = FALSE, abbr = TRUE) {
        check_bool(label)
        check_bool(abbr)
        if (!label) {
          sql_glue("EXTRACT(MONTH FROM {x})")
        } else {
          if (abbr) {
            sql_glue("TO_CHAR({x}, 'Mon')")
          } else {
            sql_glue("TO_CHAR({x}, 'Month')")
          }
        }
      },
      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        check_bool(with_year)
        check_unsupported_arg(fiscal_start, 1, backend = "PostgreSQL")

        if (with_year) {
          sql_glue(
            "(EXTRACT(YEAR FROM {x}) || '.' || EXTRACT(QUARTER FROM {x}))"
          )
        } else {
          sql_glue("EXTRACT(QUARTER FROM {x})")
        }
      },
      isoyear = function(x) {
        sql_glue("EXTRACT(YEAR FROM {x})")
      },

      seconds = function(x) postgres_period(x, "secs"),
      minutes = function(x) postgres_period(x, "mins"),
      hours = function(x) postgres_period(x, "hours"),
      days = function(x) postgres_period(x, "days"),
      weeks = function(x) postgres_period(x, "weeks"),
      months = function(x) postgres_period(x, "months"),
      years = function(x) postgres_period(x, "years"),

      # https://www.postgresql.org/docs/current/functions-datetime.html#FUNCTIONS-DATETIME-TRUNC
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
            "year"
          )
        )
        sql_glue("DATE_TRUNC({unit}, {x})")
      },

      # clock ---------------------------------------------------------------
      add_days = function(x, n, ...) {
        check_dots_empty()
        sql_glue("({x} + {n}*INTERVAL'1 day')")
      },
      add_years = function(x, n, ...) {
        check_dots_empty()
        sql_glue("({x} + {n}*INTERVAL'1 year')")
      },
      date_build = function(year, month = 1L, day = 1L, ..., invalid = NULL) {
        check_unsupported_arg(invalid, allow_null = TRUE)
        sql_glue("MAKE_DATE({year}, {month}, {day})")
      },
      date_count_between = function(start, end, precision, ..., n = 1L) {
        check_dots_empty()
        check_unsupported_arg(precision, allowed = "day")
        check_unsupported_arg(n, allowed = 1L)

        sql_glue("{end} - {start}")
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

      difftime = function(time1, time2, tz, units = "days") {
        check_unsupported_arg(tz)
        check_unsupported_arg(units, allowed = "days")

        sql_glue("(CAST({time1} AS DATE) - CAST({time2} AS DATE))")
      },
    ),
    sql_translator(
      .parent = base_agg,
      cor = sql_aggregate_2("CORR"),
      cov = sql_aggregate_2("COVAR_SAMP"),
      sd = sql_aggregate("STDDEV_SAMP", "sd"),
      var = sql_aggregate("VAR_SAMP", "var"),
      all = sql_aggregate("BOOL_AND", "all"),
      any = sql_aggregate("BOOL_OR", "any"),
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        sql_glue("STRING_AGG({x}, {collapse})")
      }
    ),
    sql_translator(
      .parent = base_win,
      cor = win_aggregate_2("CORR"),
      cov = win_aggregate_2("COVAR_SAMP"),
      sd = win_aggregate("STDDEV_SAMP"),
      var = win_aggregate("VAR_SAMP"),
      all = win_aggregate("BOOL_AND"),
      any = win_aggregate("BOOL_OR"),
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        win_over(
          sql_glue("STRING_AGG({x}, {collapse})"),
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
sql_expr_matches.sql_dialect_postgres <- function(con, x, y, ...) {
  # https://www.postgresql.org/docs/current/functions-comparison.html
  sql_glue2(con, "{x} IS NOT DISTINCT FROM {y}")
}

# http://www.postgresql.org/docs/9.3/static/sql-explain.html
#' @export
sql_query_explain.sql_dialect_postgres <- function(
  con,
  sql,
  format = "text",
  ...
) {
  format <- match.arg(format, c("text", "json", "yaml", "xml"))

  if (!is.null(format)) {
    sql_glue2(con, "EXPLAIN (FORMAT {format}) {.sql sql}")
  } else {
    sql_glue2(con, "EXPLAIN {.sql sql}")
  }
}

#' @export
sql_query_insert.sql_dialect_postgres <- function(
  con,
  table,
  from,
  insert_cols,
  by,
  conflict = c("error", "ignore"),
  ...,
  returning_cols = NULL,
  method = NULL
) {
  table <- as_table_path(table, con)

  check_string(method, allow_null = TRUE)
  method <- method %||% "on_conflict"
  arg_match(method, c("on_conflict", "where_not_exists"), error_arg = "method")
  if (method == "where_not_exists") {
    return(NextMethod("sql_query_insert"))
  }

  # https://stackoverflow.com/questions/17267417/how-to-upsert-merge-insert-on-duplicate-update-in-postgresql
  # https://www.sqlite.org/lang_UPSERT.html
  conflict <- rows_check_conflict(conflict)

  parts <- rows_insert_prep(con, table, from, insert_cols, by, lvl = 0)
  by_sql <- sql_escape_ident(con, by)

  clauses <- list(
    parts$insert_clause,
    sql_clause_select(sql("*")),
    sql_clause_from(parts$from),
    sql_clause("ON CONFLICT", by_sql, parens = TRUE),
    {
      if (conflict == "ignore") sql("DO NOTHING")
    },
    sql_returning_cols(con, returning_cols, table)
  )
  sql_format_clauses(clauses, lvl = 0)
}

#' @export
sql_query_upsert.sql_dialect_postgres <- function(
  con,
  table,
  from,
  by,
  update_cols,
  ...,
  returning_cols = NULL,
  method = NULL
) {
  check_string(method, allow_null = TRUE)
  method <- method %||% "on_conflict"
  arg_match(method, c("cte_update", "on_conflict"), error_arg = "method")

  if (method == "cte_update") {
    return(NextMethod("sql_query_upsert"))
  }

  sql_query_upsert_on_conflict(
    con,
    table,
    from,
    by,
    update_cols,
    returning_cols = returning_cols
  )
}

# Shared upsert implementation for Postgres and SQLite (ON CONFLICT syntax)
# https://stackoverflow.com/questions/17267417/how-to-upsert-merge-insert-on-duplicate-update-in-postgresql
# https://www.sqlite.org/lang_UPSERT.html
sql_query_upsert_on_conflict <- function(
  con,
  table,
  from,
  by,
  update_cols,
  returning_cols = NULL
) {
  table <- as_table_path(table, con)
  parts <- rows_prep(con, table, from, by, lvl = 0)

  insert_cols <- c(by, update_cols)
  insert_cols_sql <- sql_escape_ident(con, insert_cols)

  update_values <- set_names(
    sql_table_prefix(con, "excluded", update_cols),
    update_cols
  )
  update_cols <- sql_escape_ident(con, update_cols)

  by_sql <- sql_escape_ident(con, by)
  table_sql <- sql_escape_table_source(con, table)
  clauses <- list(
    sql_clause_insert(insert_cols_sql, into = table_sql),
    sql_clause_select(insert_cols_sql),
    sql_clause_from(parts$from),
    # `WHERE true` is required for SQLite
    sql("WHERE true"),
    sql_clause("ON CONFLICT", by_sql, parens = TRUE),
    sql("DO UPDATE"),
    sql_clause_set(update_cols, update_values),
    sql_returning_cols(con, returning_cols, table)
  )
  sql_format_clauses(clauses, lvl = 0)
}

#' @export
sql_values_subquery.sql_dialect_postgres <- sql_values_subquery_column_alias

#' @export
sql_escape_date.sql_dialect_postgres <- function(con, x) {
  sql(paste0(sql_quote(as.character(x), "'"), "::date"))
}

#' @export
sql_escape_datetime.sql_dialect_postgres <- function(con, x) {
  x <- strftime(x, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  sql(paste0(sql_quote(x, "'"), "::timestamp"))
}

#' @export
db_col_types.PqConnection <- function(con, table, call) {
  table <- as_table_path(table, con, error_call = call)

  sql <- sql_glue2(con, "SELECT * FROM {.tbl table} LIMIT 0")
  res <- DBI::dbSendQuery(con, sql)
  on.exit(DBI::dbClearResult(res))

  DBI::dbFetch(res, n = 0)
  col_info_df <- DBI::dbColumnInfo(res)
  set_names(col_info_df[[".typname"]], col_info_df[["name"]])
}

#' @export
db_col_types.PostgreSQL <- db_col_types.PqConnection
