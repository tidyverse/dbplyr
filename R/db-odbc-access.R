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
                   as.integer    = sql_cast_access("Int"),
                   as.logical    = sql_cast_access("CBool"),
                   as.character  = sql_cast_access("CStr"),
                   as.Date       = sql_cast_access("CDate"),

                   # Math
                   exp           = sql_prefix("EXP"),
                   log           = sql_prefix("LOG"),
                   log10         = function(x) {
                     build_sql("LOG(", x, ") / LOG(10)")
                   },
                   sqrt          = sql_prefix("SQR"),
                   sign          = sql_prefix("SGN"),
                   floor         = sql_prefix("INT"),
                   # If x was a whole number, this keeps it from rounding up
                   # All other decimals are correctly ceiling-ed
                   ceiling       = function(x) {
                     build_sql("INT(", x, " + .9999999999)")
                   },
                   ceil          = function(x) {
                     build_sql("INT(", x, " + .9999999999)")
                   },
                   # There is no POWER function in Access. It takes ^ instead
                   `^`           = function(x, y) {
                     build_sql(x, " ^ ", y)
                   },


                   # Trig
                   # Most implementations come from: https://en.wikibooks.org/wiki/Visual_Basic/Simple_Arithmetic#Trigonometrical_Functions
                   atan          = sql_prefix("ATN"),
                   # Catch divide by zero when x = -1 or 1 and return 0 or Pi
                   acos          = function(x) {
                     build_sql("IIF(ABS(", x, ") = 1,",                                     # If x is 1 or -1
                               x, "* -2 * ATN(1) + 2 * ATN(1),",                            # Return 0 (if 1) or Pi (if -1) using trig manipulation
                               "ATN(-", x, "/ SQR(-", x, "*", x, " + 1)) + 2 * ATN(1))")    # Otherwise return ACOS(x) using trig manipulation of ATAN
                   },
                   # Inverse hyperbolic cosine for the domain x > 1 satisfies log(x + sqrt(x^2 - 1)). x <= 1 returns complex results
                   acosh         = function(x) {
                     build_sql("LOG(", x, " + SQR(", x, "^2 - 1))")
                   },
                   # Catch divide by zero when x = -1 or 1 and return pi/2 or -pi/2
                   asin          = function(x) {
                     build_sql("IIF(ABS(", x, ") = 1,",                                     # If x is 1 or -1
                               x, "* 2 * ATN(1),",                                          # Return pi/2 (if 1) or -pi/2 (if -1) using trig manipulation
                               "ATN(", x, "/ SQR(-", x, "*", x, " + 1)))")                  # Otherwise return ASIN(x) using trig manipulation of ATAN
                   },
                   # Inverse hyperbolic sin for the domain x > 1 satisfies log(x + sqrt(x^2 + 1)). x <= 1 returns complex results
                   asinh         = function(x) {
                     build_sql("LOG(", x, " + SQR(", x, "^2 + 1))")
                   },
                   # Note that R takes y then x as arguments
                   atan2         = function(y, x) {
                     build_sql("IIF(", y, " > 0,",
                                    "IIF(", x, " >= ", y, ", ",
                                         "ATN(", y, " / ", x, "), ",
                                         "IIF(", x, " <= -", y, ", ",
                                              "ATN(", y, " / ", x, "+ ATN(1) * 4), ",
                                              "ATN(1) * 2 - ATN(", x, " / ", y, "))), ",
                                    "IIF(", x, ">= -", y, ", ",
                                         "ATN(", y, "/", x, "), ",
                                         "IIF(", x, "<=", y, ", ",
                                              "ATN(", y, "/", x, "- ATN(1)*4), ",
                                              "-ATN(1)*2 - ATN(", x, "/", y, "))))")
                   },
                   # Inverse hyperbolic tan for the domain -1 < x < 1 satisfies log((1 + x) / (1 - x)) / 2.
                   atanh         = function(x) {
                     build_sql("LOG((1 + ", x, ") / (1 - ", x, ")) / 2")
                   },
                   cot           = function(x) {
                     build_sql("1 / TAN(", x, ")")
                   },
                   coth          = function(x) {
                     build_sql("(EXP(", x, ") + Exp(-", x, ")) / (Exp(", x, ") - Exp(-", x, "))")
                   },


                   # Strings
                   nchar         = sql_prefix("LEN"),
                   tolower       = sql_prefix("LCASE"),
                   toupper       = sql_prefix("UCASE"),
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

                   # Coalesce doesn't exist in Access.
                   # NZ() only works in Access, not with the Access driver
                   # IIF(ISNULL()) is the best way to construct this
                   coalesce      = function(x, y) {
                     build_sql("IIF(ISNULL(", x, "),", y, ",", x, ")")
                   },

                   # pmin for 2 columns
                   pmin          = function(x, y) {
                     build_sql("IIF(", x, " <= ", y, ",", x, ",", y, ")")
                   },

                   pmax          = function(x, y) {
                     build_sql("IIF(", x, " <= ", y, ",", y, ",", x, ")")
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
                   cov           = sql_not_supported("cov()"),

                   # Count
                   # Count(Distinct *) does not work in Access
                   # SELECT Count(*) FROM (SELECT DISTINCT * FROM table_name) AS T <- this would work but we don't know the table name
                   n_distinct    = sql_not_supported("n_distinct")
    ),

    # Window functions not supported in Access
    sql_translator(.parent = base_odbc_win,

                   sd            = win_absent("sd"),
                   n             = win_absent("n"),
                   n_distinct    = win_absent("n_distinct"),
                   count         = win_absent("count"),
                   cor           = win_absent("cor"),
                   cov           = win_absent("cov"),
                   row_number    = win_absent("row_number"),
                   min_rank      = win_absent("min_rank"),
                   rank          = win_absent("rank"),
                   dense_rank    = win_absent("dense_rank"),
                   percent_rank  = win_absent("percent_rank"),
                   cume_dist     = win_absent("cume_dist"),
                   ntile         = win_absent("ntile"),
                   first         = win_absent("first"),
                   last          = win_absent("last"),
                   nth           = win_absent("nth"),
                   lead          = win_absent("lead"),
                   lag           = win_absent("lag"),
                   mean          = win_absent("mean"),
                   var           = win_absent("var"),
                   sum           = win_absent("sum"),
                   min           = win_absent("min"),
                   max           = win_absent("max"),
                   cummean       = win_absent("cummean"),
                   cumsum        = win_absent("cumsum"),
                   cummin        = win_absent("cummin"),
                   cummax        = win_absent("cummax")

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
