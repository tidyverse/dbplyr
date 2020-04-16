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

    # Teradata uses the TOP statement instead of LIMIT which is what SQL92 uses
    # TOP is expected after DISTINCT and not at the end of the query
    # e.g: SELECT TOP 100 * FROM my_table
    if (!is.null(limit) && !identical(limit, Inf)) {
      build_sql(" TOP ", as.integer(limit), " ", con = con)
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
sql_translate_env.Teradata <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      `!=`          = sql_infix("<>"),
      bitwNot       = sql_prefix("BITNOT", 1),
      bitwAnd       = sql_prefix("BITAND", 2),
      bitwOr        = sql_prefix("BITOR", 2),
      bitwXor       = sql_prefix("BITXOR", 2),
      bitwShiftL    = sql_prefix("SHIFTLEFT", 2),
      bitwShiftR    = sql_prefix("SHIFTRIGHT", 2),
      as.numeric    = sql_cast("NUMERIC"),
      as.double     = sql_cast("NUMERIC"),
      as.character  = sql_cast("VARCHAR(MAX)"),
      log10         = sql_prefix("LOG"),
      log           = sql_log(),
      cot           = sql_cot(),
      quantile = sql_quantile("APPROX_PERCENTILE"),
      median = sql_median("APPROX_PERCENTILE"),
      nchar         = sql_prefix("CHARACTER_LENGTH"),
      ceil          = sql_prefix("CEILING"),
      ceiling       = sql_prefix("CEILING"),
      atan2         = function(x, y) {
                        sql_expr(ATAN2(!!y, !!x))
                      },
      substr        = function(x, start, stop) {
                        len <- stop - start + 1
                        sql_expr(SUBSTR(!!x, !!start, !!len))
                      },
      paste         =  function(...) {
                        stop(
                          "`paste()`` is not supported in this SQL variant, try `paste0()` instead",
                          call. = FALSE
                        )
                      }
    ),
    sql_translator(.parent = base_odbc_agg,
      cor           = sql_not_supported("cor()"),
      cov           = sql_not_supported("cov()"),
      var           = sql_prefix("VAR_SAMP"),
    ),
    sql_translator(.parent = base_odbc_win,
      cor           = win_absent("cor"),
      cov           = win_absent("cov"),
      var           = win_recycled("VAR_SAMP")
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

utils::globalVariables(c("ATAN2", "SUBSTR"))
