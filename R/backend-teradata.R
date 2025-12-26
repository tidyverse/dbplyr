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
#' lf |> head()
NULL

#' @export
#' @rdname backend-teradata
simulate_teradata <- function() simulate_dbi("Teradata")

#' @export
dbplyr_edition.Teradata <- function(con) {
  2L
}

#' @export
sql_query_select.Teradata <- function(
  con,
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
  lvl = 0
) {
  # #685
  # https://docs.teradata.com/r/2_MC9vCtAJRlKle2Rpb0mA/frQm7Rn09FJZZLQAuaUvJA
  # You cannot specify these options in a SELECT statement that specifies the TOP n operator:
  # * DISTINCT option
  # * QUALIFY clause
  # * SAMPLE clause
  # * WITH clause

  limit_needs_subquery <- is_true(distinct)

  if (!is_null(limit) && limit_needs_subquery) {
    unlimited_query <- sql_select_clauses(
      con,
      select = sql_clause_select(select, distinct, top = NULL),
      from = sql_clause_from(from),
      where = sql_clause_where(where),
      group_by = sql_clause_group_by(group_by),
      having = sql_clause_having(having),
      window = sql_clause_window(window),
      order_by = sql_clause_order_by(order_by, subquery, limit = NULL),
      lvl = lvl + 1
    )

    alias <- unique_subquery_name()
    from <- sql_query_wrap(con, unlimited_query, name = alias)
    select_outer <- sql_star(con, alias)
    out <- sql_select_clauses(
      con,
      select = sql_clause_select(select_outer, distinct = FALSE, top = limit),
      from = sql_clause_from(from),
      where = NULL,
      group_by = NULL,
      having = NULL,
      window = NULL,
      order_by = NULL,
      lvl = lvl
    )

    return(out)
  }

  sql_select_clauses(
    con,
    select = sql_clause_select(select, distinct, top = limit),
    from = sql_clause_from(from),
    where = sql_clause_where(where),
    group_by = sql_clause_group_by(group_by),
    having = sql_clause_having(having),
    window = sql_clause_window(window),
    order_by = sql_clause_order_by(order_by, subquery, limit),
    lvl = lvl
  )
}

#' @export
sql_translation.Teradata <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_odbc_scalar,
      `!=` = sql_infix("<>"),
      bitwNot = sql_prefix("BITNOT", 1),
      bitwAnd = sql_prefix("BITAND", 2),
      bitwOr = sql_prefix("BITOR", 2),
      bitwXor = sql_prefix("BITXOR", 2),
      bitwShiftL = sql_prefix("SHIFTLEFT", 2),
      bitwShiftR = sql_prefix("SHIFTRIGHT", 2),
      as.numeric = function(x, digits = 9L) {
        digits <- vctrs::vec_cast(digits, integer())
        sql_glue("CAST({x} AS DECIMAL(12, {digits}))")
      },
      as.double = sql_cast("FLOAT"),
      as.character = function(x, nchar = 255L) {
        check_number_whole(nchar, min = 0, max = 64000)
        nchar <- vctrs::vec_cast(nchar, integer())
        sql_glue("CAST({x} AS VARCHAR({nchar}))")
      },
      as.Date = teradata_as_date,
      log10 = sql_prefix("LOG"),
      log = sql_log(),
      cot = sql_cot(),
      quantile = sql_quantile("APPROX_PERCENTILE"),
      median = sql_median("APPROX_PERCENTILE"),
      nchar = sql_prefix("CHARACTER_LENGTH"),
      ceil = sql_prefix("CEILING"),
      ceiling = sql_prefix("CEILING"),
      atan2 = function(x, y) {
        sql_glue("ATAN2({y}, {x})")
      },
      substr = function(x, start, stop) {
        len <- stop - start + 1
        sql_glue("SUBSTR({x}, {start}, {len})")
      },
      startsWith = function(string, pattern) {
        sql_glue(
          "CAST(CASE WHEN INSTR({string}, {pattern}) = 1 THEN 1 ELSE 0 END AS INTEGER)"
        )
      },
      paste = sql_paste_infix(" ", "||", function(x) x),
      paste0 = sql_paste_infix("", "||", function(x) x),
      row_number = win_rank("ROW_NUMBER", empty_order = TRUE),

      # lubridate ---------------------------------------------------------------
      # https://en.wikibooks.org/wiki/SQL_Dialects_Reference/Functions_and_expressions/Date_and_time_functions
      as_date = teradata_as_date,
      week = function(x) {
        sql_glue("WEEKNUMBER_OF_YEAR({x}, 'iso')")
      },
      quarter = function(x) {
        sql_glue("TO_CHAR({x}, 'q')")
      }
    ),
    sql_translator(
      .parent = base_odbc_agg,
      var = sql_aggregate("VAR_SAMP", "var"),
      row_number = win_rank("ROW_NUMBER", empty_order = TRUE),
      weighted.mean = function(x, w, na.rm = T) {
        # nocov start
        check_unsupported_arg(na.rm, allowed = TRUE)
        win_over(
          sql_glue("SUM(({x} * {w})) / SUM({w})"),
          win_current_group(),
          win_current_group(),
          win_current_frame()
        )
        # nocov end
      }
    ),
    sql_translator(
      .parent = base_odbc_win,
      var = win_recycled("VAR_SAMP"),
      row_number = win_rank("ROW_NUMBER", empty_order = TRUE),
      lead = function(x, n = 1L, default = NA, order_by = NULL) {
        win_over(
          sql_glue("LEAD({x}, {n}, {default})"),
          win_current_group(),
          order_by %||% win_current_group(),
          win_current_frame()
        )
      },
      lag = function(x, n = 1L, default = NA, order_by = NULL) {
        n <- as.integer(n)
        win_over(
          sql_glue("LAG({x}, {n}, {default})"),
          win_current_group(),
          order_by %||% win_current_group(),
          win_current_frame()
        )
      },
      cumsum = function(x, order_by = NULL, frame = c(-Inf, 0)) {
        win_over(
          sql_glue("SUM({x})"),
          win_current_group(),
          order_by %||% win_current_group(),
          frame %||% win_current_frame()
        )
      },
      weighted.mean = function(x, w, na.rm = T) {
        # nocov start
        check_unsupported_arg(na.rm, allowed = TRUE)
        win_over(
          sql_glue("SUM(({x} * {w})) / SUM({w})"),
          win_current_group(),
          win_current_group(),
          win_current_frame()
        )
        # nocov end
      }
    )
  )
}

teradata_as_date <- function(x) {
  xq <- enquo(x)
  x_expr <- quo_get_expr(xq)
  if (is.character(x_expr) && !is.sql(x_expr)) {
    sql_glue("DATE {x}")
  } else {
    sql_cast("DATE")(x)
  }
}

#' @export
sql_table_analyze.Teradata <- function(con, table, ...) {
  # https://www.tutorialspoint.com/teradata/teradata_statistics.htm
  sql_glue2(con, "COLLECT STATISTICS {.tbl table}")
}
