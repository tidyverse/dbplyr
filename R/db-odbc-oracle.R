#' @export
sql_select.Oracle<- function(con, select, from, where = NULL,
                             group_by = NULL, having = NULL,
                             order_by = NULL,
                             limit = NULL,
                             distinct = FALSE,
                             ...) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
                  "limit")

  out$select    <- sql_clause_select(select, con, distinct)
  out$from      <- sql_clause_from(from, con)
  out$where     <- sql_clause_where(where, con)
  out$group_by  <- sql_clause_group_by(group_by, con)
  out$having    <- sql_clause_having(having, con)
  out$order_by  <- sql_clause_order_by(order_by, con)

  # Using Oracle's FETCH FIRST SQL command instead of LIMIT
  # https://oracle-base.com/articles/12c/row-limiting-clause-for-top-n-queries-12cr1
  if (!is.null(limit) && !identical(limit, Inf)) {
    assertthat::assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
    out$limit <- build_sql(
      "FETCH FIRST ", sql(format(trunc(limit), scientific = FALSE)), " ROWS ONLY ",
      con = con
    )
  }

  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}


#' @export
sql_translate_env.Oracle <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      # Data type conversions are mostly based on this article
      # https://docs.oracle.com/cd/B19306_01/server.102/b14200/sql_elements001.htm
      as.character  = sql_cast("VARCHAR(255)"),
      as.numeric    = sql_cast("NUMBER"),
      as.double     = sql_cast("NUMBER"),
      is.null       = function(x){
                        build_sql(
                          "CASE WHEN", x ," IS NULL THEN 1 ELSE 0 END "
                        )},
      is.na         = function(x){
                        build_sql(
                          "CASE WHEN", x ," IS NULL THEN 1 ELSE 0 END "
                        )}
    ),
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
db_analyze.Oracle <- function(con, table, ...) {
  sql <- dbplyr::build_sql(
    # Using ANALYZE TABLE instead of ANALYZE as recommended in this article: https://docs.oracle.com/cd/B28359_01/server.111/b28310/general002.htm#ADMIN11524
    "ANALYZE TABLE ",
    ident(table)
    , con = con)
  DBI::dbExecute(con, sql)
}

#' @export
sql_subquery.Oracle <- function(con, from, name = unique_name(), ...) {
  # Table aliases in Oracle should not have an "AS": https://www.techonthenet.com/oracle/alias.php
  if (is.ident(from)) {
    build_sql("(", from, ") ", if(!is.null(name))ident(name) , con = con)
  } else {
    build_sql("(", from, ") ", ident(name %||% random_table_name()), con = con)
  }
}
