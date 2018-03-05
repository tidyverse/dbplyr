#' @export
`sql_select.Microsoft SQL Server`<- function(con, select, from, where = NULL,
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

    # MS SQL uses the TOP statement instead of LIMIT which is what SQL92 uses
    # TOP is expected after DISTINCT and not at the end of the query
    # e.g: SELECT TOP 100 * FROM my_table
    if (!is.null(limit) && !identical(limit, Inf)) {
      assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
      build_sql(" TOP ", as.integer(limit), " ")},

    escape(select, collapse = ", ", con = con)
  )

  out$from      <- sql_clause_from(from, con)
  out$where     <- sql_clause_where(where, con)
  out$group_by  <- sql_clause_group_by(group_by, con)
  out$having    <- sql_clause_having(having, con)
  out$order_by  <- sql_clause_order_by(order_by, con)


  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

#' @export
`sql_translate_env.Microsoft SQL Server` <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,

      `!`           = mssql_not_sql_prefix(),

      `!=`           = mssql_logical_infix("!="),
      `==`           = mssql_logical_infix("="),
      `<`            = mssql_logical_infix("<"),
      `<=`           = mssql_logical_infix("<="),
      `>`            = mssql_logical_infix(">"),
      `>=`           = mssql_logical_infix(">="),

      `&`            = mssql_generic_infix("&", "AND"),
      `&&`           = mssql_generic_infix("&", "AND"),
      `|`            = mssql_generic_infix("|", "OR"),
      `||`           = mssql_generic_infix("|", "OR"),

      `if`           = mssql_sql_if,
      if_else        = function(condition, true, false) mssql_sql_if(condition, true, false),
      ifelse         = function(test, yes, no) mssql_sql_if(test, yes, no),

      as.numeric    = sql_cast("NUMERIC"),
      as.double     = sql_cast("NUMERIC"),
      as.character  = sql_cast("VARCHAR(MAX)"),
      log           = sql_prefix("LOG"),
      nchar         = sql_prefix("LEN"),
      atan2         = sql_prefix("ATN2"),
      ceil          = sql_prefix("CEILING"),
      ceiling       = sql_prefix("CEILING"),
      substr        = function(x, start, stop){
                        len <- stop - start + 1
                        build_sql(
                          "SUBSTRING(", x, ", ", start, ", ", len, ")"
                        )},
      is.null       = function(x) mssql_is_null(x, sql_current_context()),
      is.na         = function(x) mssql_is_null(x, sql_current_context()),
                      # TRIM is not supported on MS SQL versions under 2017
                      # https://docs.microsoft.com/en-us/sql/t-sql/functions/trim-transact-sql
                      # Best solution was to nest a left and right trims.
      trimws        = function(x){
                          build_sql(
                            "LTRIM(RTRIM(", x ,"))"
                          )},
                      # MSSQL supports CONCAT_WS in the CTP version of 2016
      paste         = sql_not_supported("paste()"),

      # stringr functions

      str_length      = sql_prefix("LEN"),
      str_locate      = function(string, pattern){
                            build_sql(
                              "CHARINDEX(", pattern, ", ", string, ")"
                            )},
      str_detect      = function(string, pattern){
                            build_sql(
                              "CHARINDEX(", pattern, ", ", string, ") > 0"
                            )}
    ),
    sql_translator(.parent = base_odbc_agg,
      sd            = sql_aggregate("STDEV"),
      var           = sql_aggregate("VAR"),
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
  sql <- build_sql(
    "UPDATE STATISTICS ",
    as.sql(table)
    , con = con
  )
  DBI::dbExecute(con, sql)
}

mssql_temp_name <- function(name, temporary){
  # check that name has prefixed '##' if temporary
  if(temporary && substr(name, 1, 1) != "#") {
    name <- paste0("##", name)
    message("Created a temporary table named: ", name)
  }
  name
}

#' @export
`db_save_query.Microsoft SQL Server` <- function(con, sql, name,
                                                 temporary = TRUE,...){

  name <- mssql_temp_name(name, temporary)

  tt_sql <- build_sql("select * into ", as.sql(name), " from (", sql, ") ", as.sql(name), con = con)

  dbExecute(con, tt_sql)

  name
}

#' @export
`db_write_table.Microsoft SQL Server`  <- function(con, table, types, values, temporary = TRUE, ...) {

  table <- mssql_temp_name(table, temporary)

  dbWriteTable(
    con,
    name = table,
    types = types,
    value = values,
    temporary = FALSE,
    row.names = FALSE
  )

  table

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
    sql_expr(convert(BIT, iif(UQ(x) %is% NULL, 1L, 0L)))
  } else {
    sql_null(x)
  }
}

mssql_not_sql_prefix <- function() {
  function(...) {
    args <- list(...)
    if (sql_current_select()) {
      build_sql(sql("~"), args)
    } else {
      build_sql(sql("NOT"), args)
    }
  }
}

mssql_logical_infix <- function(f) {
  assert_that(is_string(f))

  f <- toupper(f)
  function(x, y) {
    condition <- build_sql(x, " ", sql(f), " ", y)
    if (sql_current_select()) {
      sql_expr(convert(BIT, iif(!!condition, 1, 0)))
    } else {
      condition
    }
  }
}

mssql_generic_infix <- function(if_select, if_filter) {
  assert_that(is_string(if_select))
  assert_that(is_string(if_filter))

  if_select <- toupper(if_select)
  if_filter <- toupper(if_filter)

  function(x, y) {
    if (sql_current_select()) {
      f <- if_select
    } else {
      f <- if_filter
    }
    build_sql(x, " ", sql(f), " ", y)
  }
}

mssql_sql_if <- function(cond, if_true, if_false = NULL) {
  build_sql(
     "CASE",
     " WHEN ((", cond, ") =  'TRUE')", " THEN (", if_true, ")",
     if (!is.null(if_false)){
       build_sql(" WHEN ((", cond, ") =  'FALSE')", " THEN (", if_false, ")")
     } else {
       build_sql(" ELSE ('')")
     },
     " END"
     )
}

globalVariables(c("BIT", "%is%", "convert", "iif"))
