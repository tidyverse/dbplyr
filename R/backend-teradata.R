#' Backend: Teradata
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Uses `TOP` instead of `LIMIT`
#' * Selection of user supplied translations
#'
#' Use `simulate_teradata()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-teradata
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_teradata())
#' lf %>% head()
NULL

#' @export
#' @rdname backend-teradata
simulate_teradata <- function() simulate_dbi("Teradata")

#' @export
dbplyr_edition.Teradata <- function(con) {
  2L
}

#' @export
sql_query_select.Teradata <- function(con,
                                      select,
                                      from,
                                      where = NULL,
                                      group_by = NULL,
                                      having = NULL,
                                      window = NULL,
                                      order_by = NULL,
                                      limit = NULL,
                                      distinct = FALSE,
                                      ...,
                                      subquery = FALSE,
                                      lvl = 0) {
  # #685
  # https://docs.teradata.com/r/2_MC9vCtAJRlKle2Rpb0mA/frQm7Rn09FJZZLQAuaUvJA
  # You cannot specify these options in a SELECT statement that specifies the TOP n operator:
  # * DISTINCT option
  # * QUALIFY clause
  # * SAMPLE clause
  # * WITH clause

  limit_needs_subquery <- is_true(distinct)

  if (!is_null(limit) && limit_needs_subquery) {
    unlimited_query <- sql_select_clauses(con,
      select    = sql_clause_select(con, select, distinct, top = NULL),
      from      = sql_clause_from(from),
      where     = sql_clause_where(where),
      group_by  = sql_clause_group_by(group_by),
      having    = sql_clause_having(having),
      window    = sql_clause_window(window),
      order_by  = sql_clause_order_by(order_by, subquery, limit = NULL),
      lvl       = lvl + 1
    )

    alias <- unique_subquery_name()
    from <- sql_query_wrap(con, unlimited_query, name = alias)
    select_outer <- sql_star(con, alias)
    out <- sql_select_clauses(con,
      select   = sql_clause_select(con, select_outer, distinct = FALSE, top = limit),
      from     = sql_clause_from(from),
      where    = NULL,
      group_by = NULL,
      having   = NULL,
      window   = NULL,
      order_by = NULL,
      lvl = lvl
    )

    return(out)
  }

  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct, top = limit),
    from      = sql_clause_from(from),
    where     = sql_clause_where(where),
    group_by  = sql_clause_group_by(group_by),
    having    = sql_clause_having(having),
    window    = sql_clause_window(window),
    order_by  = sql_clause_order_by(order_by, subquery, limit),
    lvl       = lvl
  )
}

#' @export
sql_translation.Teradata <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      `!=`          = sql_infix("<>"),
      bitwNot       = sql_prefix("BITNOT", 1),
      bitwAnd       = sql_prefix("BITAND", 2),
      bitwOr        = sql_prefix("BITOR", 2),
      bitwXor       = sql_prefix("BITXOR", 2),
      bitwShiftL    = sql_prefix("SHIFTLEFT", 2),
      bitwShiftR    = sql_prefix("SHIFTRIGHT", 2),
      as.numeric    = function(x, digits = 9L) {
                        digits <- vctrs::vec_cast(digits, integer())
                        sql_expr(CAST(!!x %as% DECIMAL(12L, !!digits)))
                      },
      as.double     = sql_cast("NUMERIC"),
      as.character  = sql_cast("VARCHAR(MAX)"),
      as.Date       = function(x) {
                        glue_sql2(sql_current_con(), "DATE {.val x}")
                      },
      log10         = sql_prefix("LOG"),
      log           = sql_log(),
      cot           = sql_cot(),
      quantile      = sql_quantile("APPROX_PERCENTILE"),
      median        = sql_median("APPROX_PERCENTILE"),
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
      startsWith    = function(string, pattern) {
                        glue_sql2(sql_current_con(), "CAST(CASE WHEN INSTR({.val string}, {.val pattern}) = 1 THEN 1 ELSE 0 END AS INTEGER)")
                      },
      paste         = sql_paste_infix(" ", "||", function(x) sql_expr(!!x)),
      paste0        = sql_paste_infix("", "||", function(x) sql_expr(!!x)),
      row_number    = win_rank_tdata("ROW_NUMBER"),
      week          = function(x){
                        sql_expr(WEEKNUMBER_OF_YEAR(!!x, 'iso'))
                      },
      quarter       = function(x) {
                        glue_sql2(sql_current_con(), "to_char({.val x}, 'q')")
                      }
    ),
    sql_translator(.parent = base_odbc_agg,
      var           = sql_aggregate("VAR_SAMP", "var"),
      row_number    = win_rank_tdata("ROW_NUMBER"),
      weighted.mean = function(x, w, na.rm = T) {
                        # nocov start
                        win_over(
                          sql_expr(SUM((!!x * !!w))/SUM(!!w)),
                          win_current_group(),
                          win_current_group(),
                          win_current_frame()
                        )
                        # nocov end
                      }
    ),
    sql_translator(.parent = base_odbc_win,
      var           = win_recycled("VAR_SAMP"),
      row_number    = win_rank_tdata("ROW_NUMBER"),
      lead          = function(x, n = 1L, default = NA, order_by = NULL) {
                        win_over(
                          sql_expr(LEAD(!!x, !!n, !!default)),
                          win_current_group(),
                          order_by %||% win_current_group(),
                          win_current_frame()
                        )
                      },
      lag           = function(x, n = 1L, default = NA, order_by = NULL) {
                        win_over(
                          sql_expr(LAG(!!x, !!as.integer(n), !!default)),
                          win_current_group(),
                          order_by %||%  win_current_group(),
                          win_current_frame()
                        )
                      },
      cumsum        = function(x, order_by = NULL, frame = c(-Inf, 0)) {
                        win_over(
                          sql_expr(SUM(!!x)),
                          win_current_group(),
                          order_by %||% win_current_group(),
                          frame %||% win_current_frame()
                        )
                      },
      weighted.mean = function(x, w, na.rm = T) {
                        # nocov start
                        win_over(
                          sql_expr(SUM((!!x * !!w))/SUM(!!w)),
                          win_current_group(),
                          win_current_group(),
                          win_current_frame()
                        )
                        # nocov end

                      }
    )

  )}

#' @export
sql_table_analyze.Teradata <- function(con, table, ...) {
  # https://www.tutorialspoint.com/teradata/teradata_statistics.htm
  glue_sql2(con, "COLLECT STATISTICS {.tbl table}")
}

utils::globalVariables(c("ATAN2", "SUBSTR", "DECIMAL", "WEEKNUMBER_OF_YEAR", "SUM"))

#' @export
#' @rdname win_over
win_rank_tdata <- function(f) {
  force(f)
  function(order_by = NULL) {
    order_by <- order_by %||% win_current_group()
    if (is_empty(order_by)) order_by <- sql("(SELECT NULL)")

    win_over(
      sql(glue("{f}()")),
      partition = win_current_group(),
      order = order_by,
      frame = win_current_frame()
    )
  }
}
