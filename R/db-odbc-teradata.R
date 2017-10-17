#' @export
sql_select.Teradata <- function(con, select, from, where = NULL,
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
sql_translate_env.Teradata <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
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
                      # MSSQL supports CONCAT_WS in the CTP version of 2016
      paste         = sql_not_supported("paste()")
    ),
    sql_translator(.parent = base_odbc_agg,
      sd            = sql_prefix("STDEV"),
      var           = sql_prefix("VAR"),
                      # MSSQL does not have function for: cor and cov
      cor           = sql_not_supported("cor()"),
      cov           = sql_not_supported("cov()")
    ),
    sql_translator(.parent = base_odbc_win,
      sd            = win_recycled("STDEV"),
      var           = win_recycled("VAR"),
      # MSSQL does not have function for: cor and cov
      cor           = win_absent("cor"),
      cov           = win_absent("cov")
    )

  )}

#' @export
db_analyze.Teradata <- function(con, table, ...) {
  # Using COLLECT STATISTICS instead of ANALYZE as recommended in this article
  # https://www.tutorialspoint.com/teradata/teradata_statistics.htm
  sql <- build_sql(
    "COLLECT STATISTICS ",
    ident(table)
    , con = con
  )
  DBI::dbExecute(con, sql)
}
