#' @export
`sql_select.Microsoft SQL Server` <- function(con, select, from, where = NULL,
                                             group_by = NULL, having = NULL,
                                             order_by = NULL,
                                             limit = NULL,
                                             distinct = FALSE,
                                             ...,
                                             bare_identifier_ok = FALSE) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by",
                  "having", "order_by","limit")

  assert_that(is.character(select), length(select) > 0L)
  out$select    <- build_sql(
    "SELECT ",

    if (distinct) sql("DISTINCT "),

    if (!is.null(limit) && !identical(limit, Inf)) {
      # MS SQL uses the TOP statement instead of LIMIT which is what SQL92 uses
      # TOP is expected after DISTINCT and not at the end of the query
      # e.g: SELECT TOP 100 * FROM my_table
      build_sql("TOP(", as.integer(limit), ") ", con = con)
    } else if (length(order_by) > 0 && bare_identifier_ok) {
      # Stop-gap measure so that a wider range of queries is supported (#276).
      # MS SQL doesn't allow ORDER BY in subqueries,
      # unless also TOP (or FOR XML) is specified.
      # Workaround: Use TOP 9223372036854775807 as this is signed 64-bit int max
      # and some versions of SQL Server such as Azure Data Warehouse don't
      # support TOP 100 PERCENT. https://stackoverflow.com/a/4971263
      sql("TOP 9223372036854775807 ")
    },

    escape(select, collapse = ", ", con = con),
    con = con
  )

  out$from      <- sql_clause_from(from, con)
  out$where     <- sql_clause_where(where, con)
  out$group_by  <- sql_clause_group_by(group_by, con)
  out$having    <- sql_clause_having(having, con)
  out$order_by  <- sql_clause_order_by(order_by, con)


  escape(unname(purrr::compact(out)), collapse = "\n", parens = FALSE, con = con)
}

#' @export
`sql_translate_env.Microsoft SQL Server` <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,

      `!`           = function(x) {
                        if (sql_current_select()) {
                          build_sql(sql("~"), list(x))
                        } else {
                          sql_expr(NOT(!!x))
                        }
                      },

      `!=`           = sql_infix("!="),
      `==`           = sql_infix("="),
      `<`            = sql_infix("<"),
      `<=`           = sql_infix("<="),
      `>`            = sql_infix(">"),
      `>=`           = sql_infix(">="),

      `&`            = mssql_generic_infix("&", "%AND%"),
      `&&`           = mssql_generic_infix("&", "%AND%"),
      `|`            = mssql_generic_infix("|", "%OR%"),
      `||`           = mssql_generic_infix("|", "%OR%"),

      bitwShiftL     = sql_not_supported("bitwShiftL"),
      bitwShiftR     = sql_not_supported("bitwShiftR"),

      `if`           = mssql_sql_if,
      if_else        = function(condition, true, false) mssql_sql_if(condition, true, false),
      ifelse         = function(test, yes, no) mssql_sql_if(test, yes, no),

      as.logical    = sql_cast("BIT"),

      as.Date       = sql_cast("DATE"),
      as.numeric    = sql_cast("FLOAT"),
      as.double     = sql_cast("FLOAT"),
      as.character  = sql_cast("VARCHAR(MAX)"),
      log           = sql_prefix("LOG"),
      atan2         = sql_prefix("ATN2"),
      ceil          = sql_prefix("CEILING"),
      ceiling       = sql_prefix("CEILING"),

      # https://dba.stackexchange.com/questions/187090
      pmin          = sql_not_supported("pmin()"),
      pmax          = sql_not_supported("pmax()"),

      is.null       = function(x) mssql_is_null(x, sql_current_context()),
      is.na         = function(x) mssql_is_null(x, sql_current_context()),

      # string functions ------------------------------------------------
      nchar = sql_prefix("LEN"),
      paste = sql_paste_infix(" ", "+", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "+", function(x) sql_expr(cast(!!x %as% text))),
      substr = sql_substr("SUBSTRING"),

      # stringr functions
      str_length = sql_prefix("LEN"),
      str_c = sql_paste_infix("", "+", function(x) sql_expr(cast(!!x %as% text))),
      # no built in function: https://stackoverflow.com/questions/230138
      str_to_title = sql_not_supported("str_to_title()"),
      str_sub = sql_str_sub("SUBSTRING", "LEN"),

      # lubridate ---------------------------------------------------------------
      # https://en.wikibooks.org/wiki/SQL_Dialects_Reference/Functions_and_expressions/Date_and_time_functions
      as_date = sql_cast("DATE"),

      # Using DATETIME2 as it complies with ANSI and ISO.
      # MS recommends DATETIME2 for new work:
      # https://docs.microsoft.com/en-us/sql/t-sql/data-types/datetime-transact-sql?view=sql-server-2017
      as_datetime = sql_cast("DATETIME2"),

      today = function() sql_expr(CAST(SYSDATETIME() %AS% DATE)),

      # https://docs.microsoft.com/en-us/sql/t-sql/functions/datepart-transact-sql?view=sql-server-2017
      year = function(x) sql_expr(DATEPART(YEAR, !!x)),
      day = function(x) sql_expr(DATEPART(DAY, !!x)),
      mday = function(x) sql_expr(DATEPART(DAY, !!x)),
      yday = function(x) sql_expr(DATEPART(DAYOFYEAR, !!x)),
      hour = function(x) sql_expr(DATEPART(HOUR, !!x)),
      minute = function(x) sql_expr(DATEPART(MINUTE, !!x)),
      second = function(x) sql_expr(DATEPART(SECOND, !!x)),

      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          sql_expr(DATEPART(MONTH, !!x))
        } else {
          if (!abbr) {
            sql_expr(DATENAME(MONTH, !!x))
          } else {
            stop("`abbr` is not supported in SQL Server translation", call. = FALSE)          }
        }
      },

      quarter = function(x, with_year = FALSE, fiscal_start = 1) {
        if (fiscal_start != 1) {
          stop("`fiscal_start` is not supported in SQL Server translation. Must be 1.", call. = FALSE)
        }

        if (with_year) {
          sql_expr((DATENAME(YEAR, !!x) + '.' + DATENAME(QUARTER, !!x)))
        } else {
          sql_expr(DATEPART(QUARTER, !!x))
        }
      },

    ),
    sql_translator(.parent = base_odbc_agg,
      sd            = sql_aggregate("STDEV", "sd"),
      var           = sql_aggregate("VAR", "var"),
                      # MSSQL does not have function for: cor and cov
      cor           = sql_not_supported("cor()"),
      cov           = sql_not_supported("cov()"),
      str_flatten = function(x, collapse = "") sql_expr(string_agg(!!x, !!collapse))

    ),
    sql_translator(.parent = base_odbc_win,
      sd            = win_aggregate("STDEV"),
      var           = win_aggregate("VAR"),
      # MSSQL does not have function for: cor and cov
      cor           = win_absent("cor"),
      cov           = win_absent("cov"),
      str_flatten = function(x, collapse = "") {
        win_over(
          sql_expr(string_agg(!!x, !!collapse)),
          partition = win_current_group(),
          order = win_current_order()
        )
      }
    )

  )}

#' @export
`sql_escape_raw.Microsoft SQL Server` <- function(con, x) {
  # SQL Server binary constants should be prefixed with 0x
  # https://docs.microsoft.com/en-us/sql/t-sql/data-types/constants-transact-sql?view=sql-server-ver15#binary-constants
  paste0(c("0x", format(x)), collapse = "")
}

#' @export
`db_analyze.Microsoft SQL Server` <- function(con, table, ...) {
  # Using UPDATE STATISTICS instead of ANALYZE as recommended in this article
  # https://docs.microsoft.com/en-us/sql/t-sql/statements/update-statistics-transact-sql
  sql <- build_sql("UPDATE STATISTICS ", as.sql(table), con = con)
  DBI::dbExecute(con, sql, immediate = TRUE)
}


# Temporary tables --------------------------------------------------------
# SQL server does not support CREATE TEMPORARY TABLE and instead prefixes
# temporary table names with #
# <https://docs.microsoft.com/en-us/previous-versions/sql/sql-server-2008-r2/ms177399%28v%3dsql.105%29#temporary-tables>


mssql_temp_name <- function(name, temporary) {
  if (temporary && substr(name, 1, 1) != "#") {
    name <- paste0("#", name)
    inform(paste0("Created a temporary table named: ", name),
      class = c("dbplyr_message_temp_table", "dbplyr_message")
    )
  }
  name
}

#' @export
`db_copy_to.Microsoft SQL Server` <- function(con, table, values,
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {
  NextMethod(
    table = mssql_temp_name(table, temporary),
    types = types,
    values = values,
    temporary = FALSE
  )
}

#' @export
`db_save_query.Microsoft SQL Server` <- function(con, sql, name,
                                                 temporary = TRUE, ...){
  name <- mssql_temp_name(name, temporary)

  # Different syntax for MSSQL: https://stackoverflow.com/q/16683758/946850
  tt_sql <- build_sql(
    "SELECT * ",
    "INTO ", as.sql(name), " ",
    "FROM (", sql, ") AS temp",
    con = con
  )
  dbExecute(con, tt_sql, immediate = TRUE)
  name

}

#' @export
`db_write_table.Microsoft SQL Server`  <- function(con, table, types, values, temporary = TRUE, ...) {
  NextMethod(
    table = mssql_temp_name(table, temporary),
    types = types,
    values = values,
    temporary = FALSE
  )
}

# `IS NULL` returns a boolean expression, so you can't use it in a result set
# the approach using casting return a bit, so you can use in a result set, but not in where.
# Microsoft documentation:  The result of a comparison operator has the Boolean data type.
# This has three values: TRUE, FALSE, and UNKNOWN. Expressions that return a Boolean data type are
# known as Boolean expressions. Unlike other SQL Server data types, a Boolean data type cannot
# be specified as the data type of  a table column or variable, and cannot be returned in a result set.
# https://docs.microsoft.com/en-us/sql/t-sql/language-elements/comparison-operators-transact-sql

mssql_is_null <- function(x, context = NULL) {
  needs_bit <- is.list(context) && !is.null(context$clause) &&
    context$clause %in% c("SELECT", "ORDER")

  if (needs_bit) {
    sql_expr(convert(BIT, iif(!!x %is% NULL, 1L, 0L)))
  } else {
    sql_is_null(x)
  }
}

mssql_generic_infix <- function(if_select, if_filter) {
  force(if_select)
  force(if_filter)

  function(x, y) {
    if (sql_current_select()) {
      f <- if_select
    } else {
      f <- if_filter
    }
    sql_call2(f, x, y)
  }
}

mssql_sql_if <- function(cond, if_true, if_false = NULL) {
  old <- set_current_context(list(clause = ""))
  on.exit(set_current_context(old), add = TRUE)
  cond <- build_sql(cond)

  sql_if(cond, if_true, if_false)
}

globalVariables(c("BIT", "CAST", "%AS%", "%is%", "convert", "DATE", "DATENAME", "DATEPART", "iif", "NOT", "SUBSTRING", "LTRIM", "RTRIM", "CHARINDEX", "SYSDATETIME", "SECOND", "MINUTE", "HOUR", "DAY", "DAYOFWEEK", "DAYOFYEAR", "MONTH", "QUARTER", "YEAR"))
