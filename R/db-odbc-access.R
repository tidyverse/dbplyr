# sql_ generics --------------------------------------------

#' @export
sql_select.ACCESS<- function(con, select, from,
                             where = NULL,  group_by = NULL,
                             having = NULL, order_by = NULL,
                             limit = NULL,  distinct = FALSE, ...) {

  out <- vector("list", 7)
  names(out) <- c("select",  "from",   "where",
                  "group_by","having", "order_by", "limit")

  assert_that(is.character(select), length(select) > 0L)

  out$select <- build_sql(

    "SELECT ",

    if (distinct) sql("DISTINCT "),

    # Access uses the TOP statement instead of LIMIT which is what SQL92 uses
    # TOP is expected after DISTINCT and not at the end of the query
    # e.g: SELECT TOP 100 * FROM my_table
    if (!is.null(limit) && !identical(limit, Inf)) {

      assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
      build_sql("TOP ", as.integer(limit), " ")},

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
sql_translate_env.ACCESS <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,

                   # Much of this translation comes from: https://www.techonthenet.com/access/functions/

                   # Conversion
                   as.numeric    = sql_cast_access("CDbl"),
                   as.double     = sql_cast_access("CDbl"),
                   as.integer    = sql_cast_access("CInt"),
                   as.logical    = sql_cast_access("CBool"),
                   as.character  = sql_cast_access("CStr"),
                   as.Date       = sql_cast_access("CDate"),

                   # Math
                   exp           = sql_prefix("EXP"),
                   log           = sql_prefix("LOG"),
                   sqrt          = sql_prefix("SQR"),
                   atan          = sql_prefix("ATN"),
                   floor         = sql_prefix("INT"),
                   # If x was a whole number, this keeps it from rounding up
                   # All other decimals are correctly ceiling-ed
                   ceiling       = function(x) {
                     build_sql("INT(", x, " + .9999999999)")
                   },
                   # There is no POWER function in Access. It takes ^ instead
                   `^`           = function(x, y) {
                     build_sql(x, " ^ ", y)
                   },

                   # Strings
                   nchar         = sql_prefix("LEN"),
                   # Pull `left` chars from the left, then `right` chars from the right to replicate substr
                   substr        = function(x, start, stop){
                     right  <- stop - start + 1
                     left   <- stop
                     build_sql(
                       "RIGHT(LEFT(", x, ", ", left, "), ", right, ")")
                   },
                   trimws        = sql_prefix("TRIM"),
                   # No support for CONCAT in Access. Maybe use &?
                   paste         = sql_not_supported("paste()"),
                   paste0        = sql_not_supported("paste0()"),

                   # Logic
                   # ISNULL() returns -1 for True and 0 for False
                   is.null       = function(x){
                     build_sql("IIF(ISNULL(", x ,"), 1, 0)")
                   },
                   is.na         = function(x){
                     build_sql("IIF(ISNULL(", x ,"), 1, 0)")
                   },
                   # IIF() is like ifelse()
                   ifelse        = function(test, yes, no){
                     build_sql("IIF(", test, ", ", yes, ", ", no, ")")
                   },

                   # Dates
                   Sys.Date      = sql_prefix("DATE")
    ),

    sql_translator(.parent = base_odbc_agg,

                   mean          = sql_prefix("AVG"),
                   sd            = sql_prefix("STDEV"),
                   var           = sql_prefix("VAR"),
                   max           = sql_prefix("MAX"),
                   min           = sql_prefix("MIN"),
                   # Access does not have function for: cor and cov
                   cor           = sql_not_supported("cor()"),
                   cov           = sql_not_supported("cov()")
    ),

    # Window functions not supported in Access
    sql_translator(.parent = base_odbc_win,

                   sd            = win_absent("sd"),
                   n             = win_absent("n"),
                   count         = win_absent("count"),
                   cor           = win_absent("cor"),
                   cov           = win_absent("cov")
    )

  )}

# db_ generics -----------------------------------

#' @export
db_analyze.ACCESS <- function(con, table, ...) {
  # Do nothing. Access doesn't support an analyze / update statistics function
}

# Helpers ----------------------------------------

# CAST() does not exist in Access.
# Each type has a C*() function e.g. CDbl(), CBool(), etc.
sql_cast_access <- function(type) {
  function(x) {
    build_sql(sql(type), "(", x, ")")
  }
}
