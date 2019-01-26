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

  # Processing limit via ROWNUM in a WHERE clause, thie method
  # is backwards & forward compatible: https://oracle-base.com/articles/misc/top-n-queries
  if (!is.null(limit) && !identical(limit, Inf)) {
    out <- escape(unname(purrr::compact(out)), collapse = "\n", parens = FALSE, con = con)
    assertthat::assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
    out <- build_sql(
      "SELECT * FROM ", sql_subquery(con, out), " WHERE ROWNUM <= ", limit,
      con = con)
  }else{
    escape(unname(purrr::compact(out)), collapse = "\n", parens = FALSE, con = con)
  }
}


#' @export
sql_translate_env.Oracle <- function(con) {
  sql_variant(
    sql_translator(.parent = base_odbc_scalar,
      # Data type conversions are mostly based on this article
      # https://docs.oracle.com/cd/B28359_01/olap.111/b28126/dml_commands_1029.htm#OLADM780
      # and the details for strptime https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
      as.Date = function(x, format = NA){
        if(is.na(format) | format == '%F' | format == '%Y-%m-%d'){
          build_sql(sql("DATE "), x)
        }
        else if(grepl(x, '%C') | grepl(x, '%e') | grepl(x, '%w') |
                grepl(x, '%W') | grepl(x, '%U') | grepl(x, '%x') |
                grepl(x, '%y') | grepl(x, '%z') | grepl(x, '%D')){
          stop("Error: One or more of the format identifiers is not curerntly supported by dbplyr for Oracle SQL")
        }
        else{
          format <- gsub('%c', '%a %b %e %H:%M:%S %Y', format) %>%
            gsub('%a', 'DY', .) %>%
            gsub('%A', 'DAY', .) %>%
            gsub('%b', 'MON', .) %>%
            gsub('%B', 'MONTH', .) %>%
            gsub('%d', 'DD', .) %>%
            gsub('%F', 'YYYY-MM-DD', .) %>%
            gsub('%h', 'MON', .) %>%
            gsub('%H', 'HH24', .) %>%
            gsub('%I', 'HH12', .) %>%
            gsub('%j', 'DDD', .) %>%
            gsub('%m', 'MM', .) %>%
            gsub('%M', 'MI', .) %>%
            gsub('%p', 'AM', .) %>%
            gsub('%R', 'HH24:MI', .) %>%
            gsub('%S', 'SS', .) %>%
            gsub('%T', 'HH24:MI:SS', .) %>%
            #special consideration is needed due to NLS regions
            # gsub('%u', 'DD', .) %>%
            gsub('%V', 'IW', .) %>%
            gsub('%X', 'HH24:MI:SS', .) %>%
            gsub('%Y', 'YYYY', .)

          build_sql(sql("TO_DATE("), x, sql(", "), format, sql(")"))

        }
      },
      # as.POSIXct = function(x){
        # deal with timezones
      # },
      # https://stackoverflow.com/questions/1171196
      as.character  = sql_cast("VARCHAR2(255)"),
      # bit64::as.integer64 can translate to BIGINT for some
      # vendors, which is equivalent to NUMBER(19) in Oracle
      # https://docs.oracle.com/cd/B19306_01/gateways.102/b14270/apa.htm
      as.integer64  = sql_cast("NUMBER(19)"),
      as.numeric    = sql_cast("NUMBER"),
      as.double     = sql_cast("NUMBER"),
      # https://docs.oracle.com/cd/B19306_01/server.102/b14200/operators003.htm#i997789
      paste = sql_paste_infix(" ", "||", function(x) sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "||", function(x) sql_expr(cast(!!x %as% text))),

      # lubridate functions https://lubridate.tidyverse.org/reference/index.html
      # Date-time helpers
      now = sql("SYSDATE"),
      today = sql("TRUNC(SYSDATE)"),
      origin = sql("DATE '1970-01-01'")

      # Other modification functions
      #round_date = ,
      #floor_date = ,
      #ceiling_date =

    ),
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
db_explain.Oracle <- function(con, sql, ...) {
  DBI::dbExecute(con, build_sql("EXPLAIN PLAN FOR ", sql, con = con))
  expl <- DBI::dbGetQuery(con, "SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY())")
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

#' @export
db_analyze.Oracle <- function(con, table, ...) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_4005.htm
  sql <- dbplyr::build_sql(
    "ANALYZE TABLE ", as.sql(table), " COMPUTE STATISTICS",
    con = con
  )
  DBI::dbExecute(con, sql)
}

#' @export
sql_subquery.Oracle <- function(con, from, name = unique_name(), ...) {
  # Table aliases in Oracle should not have an "AS": https://www.techonthenet.com/oracle/alias.php
  if (is.ident(from)) {
    build_sql("(", from, ") ", if (!is.null(name)) ident(name), con = con)
  } else {
    build_sql("(", from, ") ", ident(name %||% random_table_name()), con = con)
  }
}

#' @export
db_drop_table.Oracle <- function(con, table, force = FALSE, ...) {
  if (db_has_table(con, table) && force) {
    # https://stackoverflow.com/questions/1799128/oracle-if-table-exists
    sql <- build_sql(
      "BEGIN ",
      "EXECUTE IMMEDIATE 'DROP TABLE ", ident(table), "';",
      "EXCEPTION WHEN OTHERS THEN IF SQLCODE != -942 THEN RAISE; END IF; ",
      "END;",
      con = con
    )
    DBI::dbExecute(con, sql)
  }
}

# registered onLoad located in the zzz.R script
setdiff.tbl_Oracle <- function(x, y, copy = FALSE, ...) {
  # Oracle uses MINUS instead of EXCEPT for this operation:
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/queries004.htm
  add_op_set_op(x, y, "MINUS", copy = copy, ...)
}

# roacle package ----------------------------------------------------------

#' @export
sql_translate_env.OraConnection <- sql_translate_env.Oracle

#' @export
sql_select.OraConnection <- sql_select.Oracle

#' @export
db_analyze.OraConnection <- db_analyze.Oracle

#' @export
sql_subquery.OraConnection <- sql_subquery.Oracle

#' @export
db_drop_table.OraConnection <- db_drop_table.Oracle

# registered onLoad located in the zzz.R script
setdiff.OraConnection <- setdiff.tbl_Oracle
