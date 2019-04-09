#' @export
`sql_select.Microsoft SQL Server` <- function(con, select, from, where = NULL,
                                             group_by = NULL, having = NULL,
                                             order_by = NULL,
                                             limit = NULL,
                                             distinct = FALSE,
                                             ...) {
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
      assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
      build_sql("TOP(", as.integer(limit), ") ", con = con)
    } else if (!is.null(order_by)) {
      # Stop-gap measure so that a wider range of queries is supported (#276).
      # MS SQL doesn't allow ORDER BY in subqueries,
      # unless also TOP (or FOR XML) is specified.
      # Workaround: Use TOP 100 PERCENT
      # https://stackoverflow.com/a/985953/946850
      sql("TOP 100 PERCENT ")
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
      as.numeric    = sql_cast("NUMERIC"),
      as.double     = sql_cast("NUMERIC"),
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
      str_sub = sql_str_sub("SUBSTRING", "LEN")
    ),
    sql_translator(.parent = base_odbc_agg,
      sd            = sql_aggregate("STDEV", "sd"),
      var           = sql_aggregate("VAR", "var"),
                      # MSSQL does not have function for: cor and cov
      cor           = sql_not_supported("cor()"),
      cov           = sql_not_supported("cov()")
    ),
    sql_translator(.parent = base_odbc_win,
      sd            = win_aggregate("STDEV"),
      var           = win_aggregate("VAR"),
      # MSSQL does not have function for: cor and cov
      cor           = win_absent("cor"),
      cov           = win_absent("cov")
    )

  )}

#' @export
`db_analyze.Microsoft SQL Server` <- function(con, table, ...) {
  # Using UPDATE STATISTICS instead of ANALYZE as recommended in this article
  # https://docs.microsoft.com/en-us/sql/t-sql/statements/update-statistics-transact-sql
  sql <- build_sql("UPDATE STATISTICS ", as.sql(table), con = con)
  DBI::dbExecute(con, sql)
}


# Temporary tables --------------------------------------------------------
# SQL server does not support CREATE TEMPORARY TABLE and instead prefixes
# temporary table names with #

mssql_temp_name <- function(name, temporary){
  # check that name has prefixed '##' if temporary
  if (temporary && substr(name, 1, 1) != "#") {
    name <- paste0("##", name)
    message("Created a temporary table named: ", name)
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
    temporary = FALSE
  )
}

#' @export
`db_save_query.Microsoft SQL Server` <- function(con, sql, name,
                                                 temporary = TRUE, ...){
  NextMethod(
    name = mssql_temp_name(name, temporary),
    temporary = FALSE
  )
}

#' @export
`db_write_table.Microsoft SQL Server`  <- function(con, table, types, values, temporary = TRUE, ...) {
  NextMethod(
    table = mssql_temp_name(table, temporary),
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

mssql_is_null <- function(x, context) {
  if (context$clause %in% c("SELECT", "ORDER")) {
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

globalVariables(c("BIT", "%is%", "convert", "iif", "NOT", "SUBSTRING", "LTRIM", "RTRIM", "CHARINDEX"))
