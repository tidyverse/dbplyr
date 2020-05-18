#' @export
db_desc.PostgreSQLConnection <- function(x) {
  info <- dbGetInfo(x)
  host <- if (info$host == "") "localhost" else info$host

  paste0("postgres ", info$serverVersion, " [", info$user, "@",
    host, ":", info$port, "/", info$dbname, "]")
}

#' @export
db_desc.PostgreSQL <- db_desc.PostgreSQLConnection

#' @export
db_desc.PqConnection <- db_desc.PostgreSQLConnection

postgres_grepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  # https://www.postgresql.org/docs/current/static/functions-matching.html#FUNCTIONS-POSIX-TABLE
  if (any(c(perl, fixed, useBytes))) {
    abort("`perl`, `fixed` and `useBytes` parameters are unsupported")
  }

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
sql_translate_env.PostgreSQLConnection <- function(con) {
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
      str_replace = function(string, pattern, replacement){
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
      },
      str_replace_all = function(string, pattern, replacement){
        sql_expr(regexp_replace(!!string, !!pattern, !!replacement, 'g'))
      },

      # lubridate functions
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
        if (fiscal_start != 1) {
          stop("`fiscal_start` is not supported in PostgreSQL translation. Must be 1.", call. = FALSE)
        }

        if (with_year) {
          sql_expr((EXTRACT(YEAR %FROM% !!x) || '.' || EXTRACT(QUARTER %FROM% !!x)))
        } else {
          sql_expr(EXTRACT(QUARTER %FROM% !!x))
        }
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
          stop("Unrecognized arguments to `wday`", call. = FALSE)
        }
      },
      yday = function(x) sql_expr(EXTRACT(DOY %FROM% !!x))
    ),
    sql_translator(.parent = base_agg,
      n = function() sql("COUNT(*)"),
      cor = sql_aggregate_2("CORR"),
      cov = sql_aggregate_2("COVAR_SAMP"),
      sd = sql_aggregate("STDDEV_SAMP", "sd"),
      var = sql_aggregate("VAR_SAMP", "var"),
      all = sql_aggregate("BOOL_AND", "all"),
      any = sql_aggregate("BOOL_OR", "any"),
      str_flatten = function(x, collapse) sql_expr(string_agg(!!x, !!collapse))
    ),
    sql_translator(.parent = base_win,
      n = function() {
        win_over(sql("COUNT(*)"), partition = win_current_group())
      },
      cor = win_aggregate_2("CORR"),
      cov = win_aggregate_2("COVAR_SAMP"),
      sd =  win_aggregate("STDDEV_SAMP"),
      var = win_aggregate("VAR_SAMP"),
      all = win_aggregate("BOOL_AND"),
      any = win_aggregate("BOOL_OR"),
      str_flatten = function(x, collapse) {
        win_over(
          sql_expr(string_agg(!!x, !!collapse)),
          partition = win_current_group(),
          order = win_current_order()
        )
      }
    )
  )
}

#' @export
sql_translate_env.PostgreSQL <- sql_translate_env.PostgreSQLConnection

#' @export
sql_translate_env.PqConnection <- sql_translate_env.PostgreSQLConnection

#' @export
sql_translate_env.Redshift <- sql_translate_env.PostgreSQLConnection


# DBI methods ------------------------------------------------------------------

# Doesn't return TRUE for temporary tables
#' @export
db_has_table.PostgreSQLConnection <- function(con, table, ...) {
  table %in% db_list_tables(con)
}

#' @export
db_begin.PostgreSQLConnection <- function(con, ...) {
  dbExecute(con, "BEGIN TRANSACTION")
}

#' @export
db_write_table.PostgreSQLConnection <- function(con, table, types, values,
                                                temporary = TRUE, ...) {

  db_create_table(con, table, types, temporary = temporary)

  if (nrow(values) == 0)
    return(NULL)

  cols <- lapply(values, escape, collapse = NULL, parens = FALSE, con = con)
  col_mat <- matrix(unlist(cols, use.names = FALSE), nrow = nrow(values))

  rows <- apply(col_mat, 1, paste0, collapse = ", ")
  values <- paste0("(", rows, ")", collapse = "\n, ")

  sql <- build_sql("INSERT INTO ", as.sql(table), " VALUES ", sql(values), con = con)
  dbExecute(con, sql)

  table
}

#' @export
db_query_fields.PostgreSQLConnection <- function(con, sql, ...) {
  fields <- build_sql(
    "SELECT * FROM ", sql_subquery(con, sql), " WHERE 0=1",
    con = con
  )

  qry <- dbSendQuery(con, fields)
  on.exit(dbClearResult(qry))

  dbGetInfo(qry)$fieldDescription[[1]]$name
}

# http://www.postgresql.org/docs/9.3/static/sql-explain.html
#' @export
db_explain.PostgreSQLConnection <- function(con, sql, format = "text", ...) {
  format <- match.arg(format, c("text", "json", "yaml", "xml"))

  exsql <- build_sql(
    "EXPLAIN ",
    if (!is.null(format)) sql(paste0("(FORMAT ", format, ") ")),
    sql,
    con = con
  )
  expl <- dbGetQuery(con, exsql)

  paste(expl[[1]], collapse = "\n")
}

#' @export
db_explain.PostgreSQL <- db_explain.PostgreSQLConnection

#' @export
db_explain.PqConnection <- db_explain.PostgreSQLConnection

globalVariables(c("strpos", "%::%", "%FROM%", "DATE", "EXTRACT", "TO_CHAR", "string_agg", "%~*%", "%~%", "MONTH", "DOY"))
