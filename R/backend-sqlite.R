#' Backend: SQLite
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Uses non-standard `LOG()` function
#' * Date-time extraction functions from lubridate
#' * Custom median translation
#' * Right and full joins are simulated using left joins
#'
#' Use `simulate_sqlite()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-sqlite
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_sqlite())
#' lf |> transmute(x = paste(c, " times"))
#' lf |> transmute(x = log(b), y = log(b, base = 2))
NULL

#' @export
#' @rdname backend-sqlite
simulate_sqlite <- function() simulate_dbi("SQLiteConnection")

dialect_sqlite <- function() {
  new_sql_dialect(
    "sqlite",
    quote_identifier = function(x) sql_quote(x, "`"),
    supports_window_clause = TRUE
  )
}

#' @export
sql_dialect.SQLiteConnection <- function(con) {
  dialect_sqlite()
}

#' @export
dbplyr_edition.SQLiteConnection <- function(con) {
  2L
}

#' @export
db_connection_describe.SQLiteConnection <- function(con, ...) {
  paste0("sqlite ", sqlite_version(), " [", con@dbname, "]")
}

#' @export
sql_query_explain.sql_dialect_sqlite <- function(con, sql, ...) {
  sql_glue2(con, "EXPLAIN QUERY PLAN {sql}")
}

#' @export
sql_query_set_op.sql_dialect_sqlite <- sql_query_set_op.sql_dialect_hive

#' @export
sql_query_upsert.sql_dialect_sqlite <- function(
  con,
  table,
  from,
  by,
  update_cols,
  ...,
  returning_cols = NULL,
  method = NULL
) {
  sql_query_upsert_on_conflict(
    con,
    table,
    from,
    by,
    update_cols,
    returning_cols = returning_cols
  )
}

sqlite_version <- function() {
  numeric_version(RSQLite::rsqliteVersion()[[2]])
}

# SQL methods -------------------------------------------------------------

#' @export
sql_translation.sql_dialect_sqlite <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_scalar,
      as.numeric = sql_cast("REAL"),
      as.double = sql_cast("REAL"),
      log = function(x, base = exp(1)) {
        if (base != exp(1)) {
          sql_glue("LOG({x}) / LOG({base})")
        } else {
          sql_glue("LOG({x})")
        }
      },
      paste = sql_paste_infix(" ", "||"),
      paste0 = sql_paste_infix("", "||"),
      # https://www.sqlite.org/lang_corefunc.html#maxoreunc
      pmin = sql_aggregate_n("MIN", "pmin"),
      pmax = sql_aggregate_n("MAX", "pmax"),

      runif = function(n = n(), min = 0, max = 1) {
        # https://stackoverflow.com/a/23785593/7529482
        sql_runif(
          "(0.5 + RANDOM() / 18446744073709551616.0)",
          n = {{ n }},
          min = min,
          max = max
        )
      },

      # lubridate,
      today = function() {
        sql_glue("DATE('now')")
      },
      now = \() sql_glue("DATETIME('now')"),
      # https://modern-sql.com/feature/extract#proprietary-strftime
      year = \(x) sql_glue("CAST(STRFTIME('%Y', {x}) AS NUMERIC)"),
      month = \(x) sql_glue("CAST(STRFTIME('%m', {x}) AS NUMERIC)"),
      mday = \(x) sql_glue("CAST(STRFTIME('%d', {x}) AS NUMERIC)"),
      day = \(x) sql_glue("CAST(STRFTIME('%d', {x}) AS NUMERIC)"),
      hour = \(x) sql_glue("CAST(STRFTIME('%H', {x}) AS NUMERIC)"),
      minute = \(x) sql_glue("CAST(STRFTIME('%M', {x}) AS NUMERIC)"),
      second = \(x) sql_glue("CAST(STRFTIME('%f', {x}) AS REAL)"),
      yday = \(x) sql_glue("CAST(STRFTIME('%j', {x}) AS NUMERIC)"),
    ),
    sql_translator(
      .parent = base_agg,
      sd = sql_aggregate("STDEV", "sd"),
      median = sql_aggregate("MEDIAN"),
      quantile = sql_not_supported("quantile"),
    ),
    if (sqlite_version() >= "3.25") {
      sql_translator(
        .parent = base_win,
        sd = win_aggregate("STDEV"),
        median = win_absent("median"),
        quantile = sql_not_supported("quantile"),
      )
    } else {
      base_no_win # nocov
    }
  )
}

#' @export
sql_escape_logical.sql_dialect_sqlite <- function(con, x) {
  y <- as.character(as.integer(x))
  y[is.na(x)] <- "NULL"
  sql(y)
}

#' @export
sql_expr_matches.sql_dialect_sqlite <- function(con, x, y, ...) {
  # https://sqlite.org/lang_expr.html#isisnot
  sql_glue2(con, "{x} IS {y}")
}

#' @export
values_prepare.sql_dialect_sqlite <- function(con, df) {
  needs_escape <- purrr::map_lgl(
    df,
    \(col) methods::is(col, "Date") || inherits(col, "POSIXct")
  )
  purrr::modify_if(
    df,
    needs_escape,
    \(col) escape(col, con = con, parens = FALSE, collapse = NULL)
  )
}
