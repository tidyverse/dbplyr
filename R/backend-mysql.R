#' Backend: MySQL/MariaDB
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * `paste()` uses `CONCAT_WS()`
#' * String translations for `str_detect()`, `str_locate()`, and
#'   `str_replace_all()`
#' * Clear error message for unsupported full joins
#'
#' Use `simulate_mysql()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-mysql
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_mysql())
#' lf |> transmute(x = paste0(d, " times"))
NULL

#' @export
#' @rdname backend-mysql
simulate_mysql <- function() simulate_dbi("MySQLConnection")

#' @export
#' @rdname backend-mysql
simulate_mariadb <- function() simulate_dbi("MariaDBConnection")

#' @export
#' @rdname backend-mysql
dialect_mariadb <- function() {
  new_sql_dialect(
    "mariadb",
    quote_identifier = function(x) sql_quote(x, "`"),
    supports_window_clause = TRUE
  )
}

#' @export
#' @rdname backend-mysql
dialect_mysql <- function() {
  new_sql_dialect(
    "mysql",
    quote_identifier = function(x) sql_quote(x, "`"),
    supports_window_clause = TRUE
  )
}

#' @export
sql_dialect.MariaDBConnection <- function(con) {
  dialect_mariadb()
}
#' @export
sql_dialect.MySQL <- function(con) {
  dialect_mysql()
}
#' @export
sql_dialect.MySQLConnection <- function(con) {
  dialect_mysql()
}

#' @export
dbplyr_edition.MariaDBConnection <- function(con) {
  2L
}
#' @export
dbplyr_edition.MySQL <- dbplyr_edition.MariaDBConnection
#' @export
dbplyr_edition.MySQLConnection <- dbplyr_edition.MariaDBConnection

#' @export
db_connection_describe.MariaDBConnection <- function(con, ...) {
  info <- DBI::dbGetInfo(con)

  paste0(
    "mysql ",
    info$serverVersion,
    " [",
    info$username,
    "@",
    info$host,
    ":",
    info$port,
    "/",
    info$dbname,
    "]"
  )
}
#' @export
db_connection_describe.MySQL <- db_connection_describe.MariaDBConnection
#' @export
db_connection_describe.MySQLConnection <- db_connection_describe.MariaDBConnection

#' @export
db_col_types.MariaDBConnection <- function(con, table, call = caller_env()) {
  sql <- sql_glue2(con, "SHOW COLUMNS FROM {.tbl table}")
  col_info_df <- DBI::dbGetQuery(con, sql)
  set_names(col_info_df[["Type"]], col_info_df[["Field"]])
}
#' @export
db_col_types.MySQL <- db_col_types.MariaDBConnection
#' @export
db_col_types.MySQLConnection <- db_col_types.MariaDBConnection

#' @export
sql_translation.sql_dialect_mariadb <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_scalar,
      # basic type casts as per:
      # https://mariadb.com/kb/en/cast/
      # https://dev.mysql.com/doc/refman/8.0/en/cast-functions.html#function_cast
      # https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Vector-objects
      as.logical = function(x) {
        sql_glue("IF({x}, TRUE, FALSE)")
      },
      as.character = sql_cast("CHAR"),
      as.numeric = sql_cast("DOUBLE"),
      as.double = sql_cast("DOUBLE"),
      as.POSIXct = sql_cast("DATETIME"),
      as_datetime = sql_cast("DATETIME"),
      # Neither MySQL nor MariaDB support CASTing to BIGINT. MariaDB may
      # silently cast an INTEGER into a BIGINT type, MySQL outright fails.
      # https://dba.stackexchange.com/a/205822
      as.integer64 = sql_cast("INTEGER"),

      runif = function(n = n(), min = 0, max = 1) {
        sql_runif("RAND()", n = {{ n }}, min = min, max = max)
      },

      # string functions ------------------------------------------------
      paste = sql_paste(" "),
      paste0 = sql_paste(""),

      # stringr
      str_c = sql_paste(""),
      # https://dev.mysql.com/doc/refman/8.0/en/regexp.html
      # NB: case insensitive by default; could use REGEXP_LIKE for MySQL,
      # but available in MariaDB. A few more details at:
      # https://www.oreilly.com/library/view/mysql-cookbook/0596001452/ch04s11.html
      str_detect = sql_infix("REGEXP"),
      str_like = function(string, pattern, ignore_case = deprecated()) {
        ignore_case <- deprecate_ignore_case(ignore_case)
        if (ignore_case) {
          sql_glue("{string} LIKE {pattern}")
        } else {
          sql_glue("{string} LIKE BINARY {pattern}")
        }
      },
      str_ilike = function(string, pattern) {
        # MySQL's LIKE is case-insensitive by default
        sql_glue("{string} LIKE {pattern}")
      },
      str_locate = function(string, pattern) {
        sql_glue("REGEXP_INSTR({string}, {pattern})")
      },
      str_replace_all = function(string, pattern, replacement) {
        sql_glue("REGEXP_REPLACE({string}, {pattern}, {replacement})")
      }
    ),
    sql_translator(
      .parent = base_agg,
      sd = sql_aggregate("STDDEV_SAMP", "sd"),
      var = sql_aggregate("VAR_SAMP", "var"),
      str_flatten = function(x, collapse = "", na.rm = FALSE) {
        sql_check_na_rm(na.rm)
        sql_glue("GROUP_CONCAT({x} SEPARATOR {collapse})")
      }
    ),
    sql_translator(
      .parent = base_win,
      sd = win_aggregate("STDDEV_SAMP"),
      var = win_aggregate("VAR_SAMP"),
      # GROUP_CONCAT not currently available as window function
      # https://mariadb.com/kb/en/library/aggregate-functions-as-window-functions/
      str_flatten = win_absent("str_flatten")
    )
  )
}

#' @export
sql_translation.sql_dialect_mysql <- function(con) {
  maria <- unclass(sql_translation(dialect_mariadb()))
  sql_variant(
    sql_translator(
      .parent = maria$scalar,
      # SIGNED INTEGER is a BIGINT; no way to cast smaller
      as.integer = sql_cast("SIGNED INTEGER"),
      as.integer64 = sql_cast("SIGNED INTEGER"),
    ),
    maria$aggregate,
    maria$window
  )
}

#' @export
sql_table_analyze.sql_dialect_mariadb <- function(con, table, ...) {
  sql_glue2(con, "ANALYZE TABLE {.tbl table}")
}
#' @export
sql_table_analyze.sql_dialect_mysql <- sql_table_analyze.sql_dialect_mariadb

#' @export
sql_query_join.sql_dialect_mariadb <- function(
  con,
  x,
  y,
  select,
  type = "inner",
  by = NULL,
  ...
) {
  if (identical(type, "full")) {
    cli_abort("MySQL does not support full joins")
  }
  NextMethod()
}
#' @export
sql_query_join.sql_dialect_mysql <- sql_query_join.sql_dialect_mariadb


#' @export
sql_expr_matches.sql_dialect_mariadb <- function(con, x, y, ...) {
  # https://dev.mysql.com/doc/refman/5.7/en/comparison-operators.html#operator_equal-to
  sql_glue2(con, "{x} <=> {y}")
}
#' @export
sql_expr_matches.sql_dialect_mysql <- sql_expr_matches.sql_dialect_mariadb

# https://modern-sql.com/blog/2018-08/whats-new-in-mariadb-10.3#3.values
# MariaDB doesn't accept `ROW` unlike MySQL
#' @export
sql_values_subquery.sql_dialect_mariadb <- sql_values_subquery.sql_dialect

#' @export
sql_values_subquery.sql_dialect_mysql <- function(
  con,
  df,
  types,
  lvl = 0,
  ...
) {
  # https://dev.mysql.com/doc/refman/8.0/en/values.html
  sql_values_subquery_default(con, df, types = types, lvl = lvl, row = TRUE)
}

#' @export
sql_query_update_from.sql_dialect_mariadb <- function(
  con,
  table,
  from,
  by,
  update_values,
  ...,
  returning_cols = NULL
) {
  table <- as_table_path(table, con)

  if (!is_empty(returning_cols)) {
    check_unsupported_arg(returning_cols, backend = "MariaDB")
  }

  # https://stackoverflow.com/a/19346375/946850
  parts <- rows_prep(con, table, from, by, lvl = 0)
  update_cols <- sql_table_prefix(con, table, names(update_values))

  table_sql <- sql_escape_table_source(con, table)
  clauses <- list(
    sql_clause_update(table_sql),
    sql_clause("INNER JOIN", parts$from),
    sql_clause_on(parts$where, lvl = 1),
    sql_clause_set(update_cols, update_values),
    sql_returning_cols(con, returning_cols, table)
  )
  sql_format_clauses(clauses, lvl = 0)
}
#' @export
sql_query_update_from.sql_dialect_mysql <- sql_query_update_from.sql_dialect_mariadb


#' @export
sql_query_upsert.sql_dialect_mariadb <- function(
  con,
  table,
  from,
  by,
  update_cols,
  ...,
  returning_cols = NULL,
  method = NULL
) {
  cli_abort("{.fun rows_upsert} is not supported for MariaDB.")
}
#' @export
sql_query_upsert.sql_dialect_mysql <- sql_query_upsert.sql_dialect_mariadb

#' @export
sql_escape_datetime.sql_dialect_mariadb <- function(con, x) {
  # DateTime format as per:
  # https://dev.mysql.com/doc/refman/8.0/en/datetime.html
  # https://mariadb.com/kb/en/datetime/
  x <- strftime(x, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  sql_escape_string(con, x)
}
#' @export
sql_escape_datetime.sql_dialect_mysql <- sql_escape_datetime.sql_dialect_mariadb
