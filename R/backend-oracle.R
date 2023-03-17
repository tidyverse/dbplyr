#' Backend: Oracle
#'
#' @description
#' See `vignette("translation-function")` and `vignette("translation-verb")` for
#' details of overall translation technology. Key differences for this backend
#' are:
#'
#' * Use `FETCH FIRST` instead of `LIMIT`
#' * Custom types
#' * `paste()` uses `||`
#' * Custom subquery generation (no `AS`)
#' * `setdiff()` uses `MINUS` instead of `EXCEPT`
#'
#' Use `simulate_oracle()` with `lazy_frame()` to see simulated SQL without
#' converting to live access database.
#'
#' @name backend-oracle
#' @aliases NULL
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' lf <- lazy_frame(a = TRUE, b = 1, c = 2, d = "z", con = simulate_oracle())
#' lf %>% transmute(x = paste0(c, " times"))
#' lf %>% setdiff(lf)
NULL

#' @export
#' @rdname backend-oracle
simulate_oracle <- function() simulate_dbi("Oracle")

#' @export
dbplyr_edition.Oracle <- function(con) {
  2L
}

#' @export
sql_query_select.Oracle <- function(con,
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

  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct),
    from      = sql_clause_from(from),
    where     = sql_clause_where(where),
    group_by  = sql_clause_group_by(group_by),
    having    = sql_clause_having(having),
    window    = sql_clause_window(window),
    order_by  = sql_clause_order_by(order_by, subquery, limit),
    # Requires Oracle 12c, released in 2013
    limit =   if (!is.null(limit)) {
      build_sql("FETCH FIRST ", as.integer(limit), " ROWS ONLY", con = con)
    },
    lvl = lvl
  )
}

#' @export
sql_query_upsert.Oracle <- function(con,
                                    x_name,
                                    y,
                                    by,
                                    update_cols,
                                    ...,
                                    returning_cols = NULL,
                                    method = NULL) {
  method <- method %||% "merge"
  arg_match(method, c("merge", "cte_update"), error_arg = "method")
  if (method == "cte_update") {
    return(NextMethod("sql_query_upsert"))
  }

  # https://oracle-base.com/articles/9i/merge-statement
  parts <- rows_prep_legacy(con, x_name, y, by, lvl = 0)
  update_cols_esc <- sql(sql_escape_ident(con, update_cols))
  update_values <- sql_table_prefix(con, update_cols, ident("excluded"))
  update_clause <- sql(paste0(update_cols_esc, " = ", update_values))
  update_cols_qual <- sql_table_prefix(con, update_cols, ident("...y"))

  clauses <- list(
    sql_clause("MERGE INTO", x_name),
    sql_clause("USING", parts$from),
    sql_clause_on(parts$where, lvl = 1),
    sql("WHEN MATCHED THEN"),
    sql_clause("UPDATE SET", update_clause, lvl = 1),
    sql("WHEN NOT MATCHED THEN"),
    sql_clause_insert(con, update_cols_esc, lvl = 1),
    sql_clause("VALUES", update_cols_qual, parens = TRUE, lvl = 1),
    sql_returning_cols(con, returning_cols, x_name),
    sql(";")
  )
  sql_format_clauses(clauses, lvl = 0, con)
}

#' @export
sql_translation.Oracle <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      # Data type conversions are mostly based on this article
      # https://docs.oracle.com/cd/B19306_01/server.102/b14200/sql_elements001.htm

      # https://stackoverflow.com/questions/1171196
      as.character  = sql_cast("VARCHAR2(255)"),
      # https://oracle-base.com/articles/misc/oracle-dates-timestamps-and-intervals
      as.Date = function(x) build_sql("DATE ", x),
      # bit64::as.integer64 can translate to BIGINT for some
      # vendors, which is equivalent to NUMBER(19) in Oracle
      # https://docs.oracle.com/cd/B19306_01/gateways.102/b14270/apa.htm
      as.integer64  = sql_cast("NUMBER(19)"),
      as.numeric    = sql_cast("NUMBER"),
      as.double     = sql_cast("NUMBER"),

      runif = function(n = n(), min = 0, max = 1) {
        sql_runif(dbms_random.VALUE(), n = {{ n }}, min = min, max = max)
      },

      # string -----------------------------------------------------------------
      # https://docs.oracle.com/cd/B19306_01/server.102/b14200/operators003.htm#i997789
      paste = sql_paste_infix(" ", "||", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),
      str_c = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),

      # lubridate --------------------------------------------------------------
      today = function() sql_expr(TRUNC(CURRENT_TIMESTAMP)),
      now = function() sql_expr(CURRENT_TIMESTAMP)
    ),
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
sql_query_explain.Oracle <- function(con, sql, ...) {
  build_sql(
    "EXPLAIN PLAN FOR ", sql, ";\n",
    "SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY()));",
    con = con
  )
}

#' @export
sql_table_analyze.Oracle <- function(con, table, ...) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_4005.htm
  build_sql("ANALYZE TABLE ", as.sql(table, con = con), " COMPUTE STATISTICS", con = con)
}

#' @export
sql_query_wrap.Oracle <- function(con, from, name = NULL, ..., lvl = 0) {
  # Table aliases in Oracle should not have an "AS": https://www.techonthenet.com/oracle/alias.php
  if (is.ident(from)) {
    build_sql("(", from, ") ", as_subquery_name(name, default = NULL), con = con)
  } else {
    build_sql(sql_indent_subquery(from, con, lvl), " ", as_subquery_name(name), con = con)
  }
}

#' @export
sql_query_save.Oracle <- function(con, sql, name, temporary = TRUE, ...) {
  build_sql(
    "CREATE ", if (temporary) sql("GLOBAL TEMPORARY "), "TABLE \n",
    as.sql(name, con), " AS\n", sql,
    con = con
  )
}

#' @export
sql_values_subquery.Oracle <- function(con, df, types, lvl = 0, ...) {
  sql_values_subquery_union(con, df, types = types, lvl = lvl, from = "DUAL")
}

# registered onLoad located in the zzz.R script
setdiff.tbl_Oracle <- function(x, y, copy = FALSE, ...) {
  # Oracle uses MINUS instead of EXCEPT for this operation:
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/queries004.htm
  x$lazy_query <- add_set_op(x, y, "MINUS", copy = copy, ...)
  x
}

#' @export
sql_expr_matches.Oracle <- function(con, x, y) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions040.htm
  build_sql("decode(", x, ", ", y, ", 0, 1) = 0", con = con)
}

#' @export
supports_star_without_alias.Oracle <- function(con) {
  FALSE
}


# roacle package ----------------------------------------------------------

#' @export
dbplyr_edition.OraConnection <- dbplyr_edition.Oracle

#' @export
sql_query_select.OraConnection <- sql_query_select.Oracle

#' @export
sql_query_upsert.OraConnection <- sql_query_upsert.Oracle

#' @export
sql_translation.OraConnection <- sql_translation.Oracle

#' @export
sql_query_explain.OraConnection <- sql_query_explain.Oracle

#' @export
sql_table_analyze.OraConnection <- sql_table_analyze.Oracle

#' @export
sql_query_wrap.OraConnection <- sql_query_wrap.Oracle

#' @export
sql_query_save.OraConnection <- sql_query_save.Oracle

#' @export
sql_values_subquery.OraConnection <- sql_values_subquery.Oracle

# registered onLoad located in the zzz.R script
setdiff.OraConnection <- setdiff.tbl_Oracle

#' @export
sql_expr_matches.OraConnection <- sql_expr_matches.Oracle

#' @export
supports_star_without_alias.OraConnection <- supports_star_without_alias.Oracle

globalVariables(c("DATE", "CURRENT_TIMESTAMP", "TRUNC", "dbms_random.VALUE"))
