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

  if (!is.null(limit)) {
    limit <- as.integer(limit)
    where = c(paste0("ROWNUM <= ", limit), where)
  }

  sql_select_clauses(con,
    select    = sql_clause_select(con, select, distinct),
    from      = sql_clause_from(from),
    where     = sql_clause_where(where),
    group_by  = sql_clause_group_by(group_by),
    having    = sql_clause_having(having),
    window    = sql_clause_window(window),
    order_by  = sql_clause_order_by(order_by, subquery, limit),
    lvl = lvl
  )
}

#' @export
sql_query_upsert.Oracle <- function(con,
                                    table,
                                    from,
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
  parts <- rows_prep(con, table, from, by, lvl = 0)
  update_cols_esc <- sql(sql_escape_ident(con, update_cols))
  update_values <- sql_table_prefix(con, update_cols, ident("...y"))
  update_clause <- sql(paste0(update_cols_esc, " = ", update_values))

  insert_cols <- c(by, update_cols)
  insert_cols_esc <- escape(ident(insert_cols), parens = FALSE, con = con)
  insert_values <- sql_table_prefix(con, insert_cols, "...y")

  clauses <- list(
    sql_clause("MERGE INTO", table),
    sql_clause("USING", parts$from),
    sql_clause_on(parts$where, lvl = 1, parens = TRUE),
    sql("WHEN MATCHED THEN"),
    sql_clause("UPDATE SET", update_clause, lvl = 1),
    sql("WHEN NOT MATCHED THEN"),
    sql_clause_insert(con, insert_cols_esc, lvl = 1),
    sql_clause("VALUES", insert_values, parens = TRUE, lvl = 1),
    sql_returning_cols(con, returning_cols, table),
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
      as.Date = function(x) glue_sql2(sql_current_con(), "DATE {x}"),
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
  glue_sql2(con, "EXPLAIN PLAN FOR {sql};")
}

#' @export
sql_table_analyze.Oracle <- function(con, table, ...) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_4005.htm
  glue_sql2(con, "ANALYZE TABLE {.tbl table} COMPUTE STATISTICS")
}

#' @export
sql_query_save.Oracle <- function(con, sql, name, temporary = TRUE, ...) {
  type <- if (temporary)  "GLOBAL TEMPORARY " else ""
  glue_sql2(con, "CREATE {type}TABLE {.tbl name} AS\n{sql}")
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
sql_expr_matches.Oracle <- function(con, x, y, ...) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions040.htm
  glue_sql2(con, "decode({x}, {y}, 0, 1) = 0")
}

#' @export
db_explain.Oracle <- function(con, sql, ...) {
  sql <- sql_query_explain(con, sql, ...)
  call <- current_call()
  tryCatch(
    {
      DBI::dbExecute(con, sql)
      expl <- DBI::dbGetQuery(
        con,
        glue_sql2(
          con,
          "SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY());"
        )
      )
    },
    error = function(cnd) {
      cli_abort("Can't explain query.", parent = cnd)
    }
  )

  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

#' @export
db_supports_table_alias_with_as.Oracle <- function(con) {
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
sql_query_save.OraConnection <- sql_query_save.Oracle

#' @export
sql_values_subquery.OraConnection <- sql_values_subquery.Oracle

# registered onLoad located in the zzz.R script
setdiff.OraConnection <- setdiff.tbl_Oracle

#' @export
sql_expr_matches.OraConnection <- sql_expr_matches.Oracle

#' @export
db_explain.OraConnection <- db_explain.Oracle

#' @export
db_supports_table_alias_with_as.OraConnection <- db_supports_table_alias_with_as.Oracle

utils::globalVariables(c("DATE", "CURRENT_TIMESTAMP", "TRUNC", "dbms_random.VALUE"))
