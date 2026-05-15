#' DB2 backend
#'
#' @description
#' This backend supports IBM DB2 databases, typically accessed via ODBC.
#' Use `dialect_db2()` with `lazy_frame()` to see simulated SQL without
#' connecting to a live database.
#'
#' Key differences for this backend are:
#'
#' * Uses `FETCH FIRST n ROWS ONLY` instead of `LIMIT n`
#' * Uses double quotes for identifier quoting
#' * `paste()` uses `||`
#' * DB2-specific data type names for casts (e.g. `VARCHAR(255)`, `DOUBLE`)
#' * Date component extraction via `YEAR()`, `MONTH()`, ..., `DAYOFYEAR()`,
#'   `DAYOFWEEK()`, `QUARTER()`, `WEEK()`
#' * `str_flatten()` uses `LISTAGG`
#' * Statistical summaries `sd()`, `var()`, `cor()`, `cov()`
#' * `runif()` translates to `RAND()`
#' * Regular expression functions (DB2 11.1+)
#'
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology.
#'
#' @name backend-db2
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = dialect_db2())
#' lf |> head()
#' lf |> transmute(x = paste0(d, " times"))
#' lf |> summarise(x = sd(b, na.rm = TRUE))
NULL

#' @export
#' @rdname backend-db2
dialect_db2 <- function() {
  new_sql_dialect(
    "db2",
    quote_identifier = function(x) sql_quote(x, '"')
  )
}

#' @export
sql_dialect.DB2Connection <- function(con) {
  dialect_db2()
}

#' @export
dbplyr_edition.DB2Connection <- function(con) {
  2L
}

#' @export
sql_query_select.sql_dialect_db2 <- function(
  con,
  select,
  from,
  where = NULL,
  group_by = NULL,
  having = NULL,
  window = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  ...,
  subquery = FALSE,
  lvl = 0
) {
  sql_select_clauses(
    select = sql_clause_select(select, distinct),
    from = sql_clause_from(from),
    where = sql_clause_where(where),
    group_by = sql_clause_group_by(group_by),
    having = sql_clause_having(having),
    window = sql_clause_window(window),
    order_by = sql_clause_order_by(order_by, subquery, limit),
    # DB2 uses FETCH FIRST instead of LIMIT
    limit = if (!is.null(limit)) {
      limit <- as.integer(limit)
      sql_glue2(con, "FETCH FIRST {limit} ROWS ONLY")
    },
    lvl = lvl
  )
}

#' @export
sql_translation.sql_dialect_db2 <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_odbc_scalar,
      # https://www.ibm.com/docs/en/db2-for-zos/12.0.0?topic=elements-casting-between-data-types
      as.character = sql_cast("VARCHAR(255)"),
      as.numeric = sql_cast("DOUBLE"),
      as.double = sql_cast("DOUBLE"),
      as.integer = sql_cast("INTEGER"),
      as.integer64 = sql_cast("BIGINT"),
      as.Date = sql_cast("DATE"),
      as.POSIXct = sql_cast("TIMESTAMP"),

      # https://www.ibm.com/docs/en/db2/9.7?topic=functions-rand
      runif = function(n = n(), min = 0, max = 1) {
        sql_runif("RAND()", n = {{ n }}, min = min, max = max)
      },

      # string -----------------------------------------------------------------
      # https://www.ibm.com/docs/en/db2-for-zos/12.0.0?topic=functions-concat
      paste = sql_paste_infix(" ", "||", sql_cast("VARCHAR(255)")),
      paste0 = sql_paste_infix("", "||", sql_cast("VARCHAR(255)")),
      str_c = sql_paste_infix("", "||", sql_cast("VARCHAR(255)")),

      # DB2 supports POSITION(substring IN string)
      # https://www.db2tutorial.com/db2-string-functions/db2-locate/
      str_locate = function(string, pattern) {
        sql_glue("LOCATE({pattern}, {string})")
      },
      str_detect = function(string, pattern, negate = FALSE) {
        sql_str_pattern_switch(
          string = string,
          pattern = {{ pattern }},
          negate = negate,
          f_fixed = sql_str_detect_fixed_position("detect")
        )
      },
      str_starts = function(string, pattern, negate = FALSE) {
        sql_str_pattern_switch(
          string = string,
          pattern = {{ pattern }},
          negate = negate,
          f_fixed = sql_str_detect_fixed_position("start")
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

      # https://www.ibm.com/docs/en/db2/11.5.x?topic=functions-regexp-replace
      # 4th argument is starting position (default: 1 => first char of string)
      # 5th argument is occurrence (default: 0 => match all occurrences)
      str_replace = function(string, pattern, replacement) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement}, 1, 1)")
      },
      str_replace_all = function(string, pattern, replacement) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement})")
      },
      str_remove = function(string, pattern) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, '', 1, 1)")
      },
      str_remove_all = function(string, pattern) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, '')")
      },
      str_squish = function(string) {
        sql_glue("LTRIM(RTRIM(REGEXP_REPLACE({string}, '\\s+', ' ')))")
      },

      # lubridate --------------------------------------------------------------
      # https://www.ibm.com/docs/en/db2/11.5.x?topic=functions-dayofweek
      today = \() sql("CURRENT DATE"),
      now = \() sql("CURRENT TIMESTAMP"),

      yday = sql_prefix("DAYOFYEAR", 1),
      wday = function(x, label = FALSE, abbr = TRUE, week_start = NULL) {
        check_bool(label)
        check_bool(abbr)
        check_number_whole(week_start, allow_null = TRUE)

        if (!label) {
          week_start <- week_start %||% getOption("lubridate.week.start", 7)
          # DB2 DAYOFWEEK: 1=Sunday, 7=Saturday
          offset <- as.integer(7 - week_start)
          sql_glue("MOD(DAYOFWEEK({x}) + {offset} - 1, 7) + 1")
        } else if (abbr) {
          sql_glue("SUBSTR(DAYNAME({x}), 1, 3)")
        } else {
          sql_glue("DAYNAME({x})")
        }
      },
      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        check_bool(with_year)
        check_unsupported_arg(fiscal_start, 1, backend = "DB2")

        if (with_year) {
          sql_glue("(CHAR(YEAR({x})) || '.' || CHAR(QUARTER({x})))")
        } else {
          sql_glue("QUARTER({x})")
        }
      },
      week = sql_prefix("WEEK_ISO", 1),
      isoweek = sql_prefix("WEEK_ISO", 1),
      isoyear = sql_prefix("YEAR", 1),

      # clock ------------------------------------------------------------------
      # https://db2portal.blogspot.com/2022/01/db2-date-and-time-arithmetic.html
      add_days = function(x, n, ...) {
        check_dots_empty()
        sql_glue("({x} + {n} DAYS)")
      },
      add_years = function(x, n, ...) {
        check_dots_empty()
        sql_glue("({x} + {n} YEARS)")
      },

      difftime = function(time1, time2, tz, units = "days") {
        check_unsupported_arg(tz)
        check_unsupported_arg(units, allowed = "days")

        sql_glue("(DAYS(CAST({time1} AS DATE)) - DAYS(CAST({time2} AS DATE)))")
      }
    ),
    sql_translator(
      .parent = base_odbc_agg,
      # https://www.ibm.com/docs/SSEPGG_11.5.0/com.ibm.db2.luw.sql.ref.doc/doc/r0002319.html
      var = sql_aggregate("VAR_SAMP", "var"),
      cor = sql_aggregate_2("CORRELATION"),
      cov = sql_aggregate_2("COVARIANCE_SAMP"),
      # https://www.db2tutorial.com/db2-aggregate-functions/db2-listagg/
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        sql_glue("LISTAGG({x}, {collapse})")
      }
    ),
    sql_translator(
      .parent = base_odbc_win,
      var = win_aggregate("VAR_SAMP"),
      cor = win_aggregate_2("CORRELATION"),
      cov = win_aggregate_2("COVARIANCE_SAMP"),
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        win_over(
          sql_glue("LISTAGG({x}, {collapse})"),
          partition = win_current_group(),
          order = win_current_order()
        )
      }
    )
  )
}

#' @export
sql_table_analyze.sql_dialect_db2 <- function(con, table, ...) {
  # https://www.ibm.com/support/knowledgecenter/en/SSEPGG_11.1.0/com.ibm.db2.luw.admin.cmd.doc/doc/r0001980.html
  sql_glue2(
    con,
    "RUNSTATS ON TABLE {.tbl table} WITH DISTRIBUTION AND DETAILED INDEXES ALL"
  )
}

#' @export
sql_values_subquery.sql_dialect_db2 <- sql_values_subquery_column_alias
