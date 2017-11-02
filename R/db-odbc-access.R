# sql_ generics --------------------------------------------

#' @export
sql_select.ACCESS <- function(con, select, from,
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
                   as.numeric    = sql_prefix("CDBL"),
                   as.double     = sql_prefix("CDBL"),
                   # as.integer() always rounds down. CInt does not, but Int does
                   as.integer    = sql_prefix("INT"),
                   as.logical    = sql_prefix("CBOOL"),
                   as.character  = sql_prefix("CSTR"),
                   as.Date       = sql_prefix("CDATE"),

                   # Math
                   exp           = sql_prefix("EXP"),
                   log           = sql_prefix("LOG"),
                   log10         = function(x) {
                     sql_expr(log(!!x) / log(10L))
                   },
                   sqrt          = sql_prefix("SQR"),
                   sign          = sql_prefix("SGN"),
                   floor         = sql_prefix("INT"),
                   # Nearly add 1, then drop off the decimal. This results in the equivalent to ceiling()
                   ceiling       = function(x) {
                     sql_expr(int(UQ(x) + .9999999999))
                   },
                   ceil          = function(x) {
                     sql_expr(int(UQ(x) + .9999999999))
                   },
                   # There is no POWER function in Access. It uses ^ instead
                   `^`           = function(x, y) {
                     sql_expr(UQ(x) ^ UQ(y))
                   },

                   # Strings
                   nchar         = sql_prefix("LEN"),
                   tolower       = sql_prefix("LCASE"),
                   toupper       = sql_prefix("UCASE"),
                   # Pull `left` chars from the left, then `right` chars from the right to replicate substr
                   substr        = function(x, start, stop){
                     right  <- stop - start + 1
                     left   <- stop
                     sql_expr(right(left(!!x, !!left), !!right))
                   },
                   trimws        = sql_prefix("TRIM"),
                   # No support for CONCAT in Access
                   paste         = sql_paste_infix(" ", "&", function(x) sql_expr(CStr(!!x))),
                   paste0        = sql_paste_infix("", "&", function(x) sql_expr(CStr(!!x))),

                   # Logic
                   # Access always returns -1 for True and 0 for False
                   is.null       = sql_prefix("ISNULL"),
                   is.na         = sql_prefix("ISNULL"),
                   # IIF() is like ifelse()
                   ifelse        = function(test, yes, no){
                     sql_expr(iif(!!test, !!yes, !!no))
                   },

                   # Coalesce doesn't exist in Access.
                   # NZ() only works while in Access, not with the Access driver
                   # IIF(ISNULL()) is the best way to construct this
                   coalesce      = function(x, y) {
                     sql_expr(iif(isnull(!!x), !!y, !!x))
                   },

                   # pmin/pmax for 2 columns
                   pmin          = function(x, y) {
                     sql_expr(iif(UQ(x) <= UQ(y), !!x, !!y))
                   },

                   pmax          = function(x, y) {
                     sql_expr(iif(UQ(x) <= UQ(y), !!y, !!x))
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
                   # Access does not have functions for cor and cov
                   cor           = sql_not_supported("cor()"),
                   cov           = sql_not_supported("cov()"),

                   # Count
                   # Count(Distinct *) does not work in Access
                   # This would work but we don't know the table name when translating:
                   # SELECT Count(*) FROM (SELECT DISTINCT * FROM table_name) AS T
                   n_distinct    = sql_not_supported("n_distinct")
    ),

    # Window functions not supported in Access
    sql_translator(.parent = base_no_win)

  )}

# db_ generics -----------------------------------

#' @export
db_analyze.ACCESS <- function(con, table, ...) {
  # Do nothing. Access doesn't support an analyze / update statistics function
}

# Util -------------------------------------------

sql_escape_logical.ACCESS <- function(con, x) {
  # Access uses a convention of -1 as True and 0 as False
  y <- ifelse(x, -1, 0)
  y[is.na(x)] <- "NULL"
  y
}

globalVariables(c("CStr", "iif", "isnull", "text"))
