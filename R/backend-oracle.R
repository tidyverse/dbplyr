#' @export
sql_select.Oracle <- function(con,
                              select,
                              from,
                              where = NULL,
                              group_by = NULL,
                              having = NULL,
                              order_by = NULL,
                              limit = NULL,
                              distinct = FALSE,
                              ...) {
  out <- vector("list", 7)
  names(out) <-
    c(
      "select",
      "from",
      "where",
      "group_by",
      "having",
      "order_by",
      "limit"
    )

  out$select <- sql_clause_select(select, con, distinct)
  out$from <- sql_clause_from(from, con)
  out$where <- sql_clause_where(where, con)
  out$group_by <- sql_clause_group_by(group_by, con)
  out$having <- sql_clause_having(having, con)
  out$order_by <- sql_clause_order_by(order_by, con)

  # Processing limit via ROWNUM in a WHERE clause, thie method
  # is backwards & forward compatible: https://oracle-base.com/articles/misc/top-n-queries
  if (!is.null(limit) && !identical(limit, Inf)) {
    out <-
      escape(
        unname(purrr::compact(out)),
        collapse = "\n",
        parens = FALSE,
        con = con
      )
    assertthat::assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
    out <- build_sql("SELECT * FROM ",
      sql_subquery(con, out),
      " WHERE ROWNUM <= ",
      limit,
      con = con
    )
  } else {
    escape(
      unname(purrr::compact(out)),
      collapse = "\n",
      parens = FALSE,
      con = con
    )
  }
}


#' @export
sql_translate_env.Oracle <- function(con) {
  sql_variant(
    sql_translator(
      .parent = base_odbc_scalar,
      # Data type conversions are mostly based on this article
      # https://docs.oracle.com/cd/B28359_01/olap.111/b28126/dml_commands_1029.htm#OLADM780
      # and the details for strptime https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
      as.Date = function(x, format = NA) {
        if (is.na(format) | format == "%F" | format == "%Y-%m-%d") {
          build_sql(sql("DATE "), x)
        }
        else if (grepl("%C", x) | grepl("%e", x) | grepl("%w", x) |
          grepl("%W", x) | grepl("%U", x) | grepl("%x", x) |
          grepl("%y", x) | grepl("%z", x) | grepl("%D", x)) {
          stop(
            "Error: One or more of the format identifiers is not curerntly supported by dbplyr for Oracle SQL"
          )
        }
        else {
          format <- gsub("%c", "%a %b %e %H:%M:%S %Y", format) %>%
            gsub("%a", "DY", .) %>%
            gsub("%A", "DAY", .) %>%
            gsub("%b", "MON", .) %>%
            gsub("%B", "MONTH", .) %>%
            gsub("%d", "DD", .) %>%
            gsub("%F", "YYYY-MM-DD", .) %>%
            gsub("%h", "MON", .) %>%
            gsub("%H", "HH24", .) %>%
            gsub("%I", "HH12", .) %>%
            gsub("%j", "DDD", .) %>%
            gsub("%m", "MM", .) %>%
            gsub("%M", "MI", .) %>%
            gsub("%p", "AM", .) %>%
            gsub("%R", "HH24:MI", .) %>%
            gsub("%S", "SS", .) %>%
            gsub("%T", "HH24:MI:SS", .) %>%
            # special consideration is needed due to NLS regions
            # gsub('%u', 'DD', .) %>%
            gsub("%V", "IW", .) %>%
            gsub("%X", "HH24:MI:SS", .) %>%
            gsub("%Y", "YYYY", .)

          build_sql(sql("TO_DATE("), x, sql(", "), format, sql(")"))
        }
      },
      # as.POSIXct = function(x){
      # deal with timezones
      # },
      # https://stackoverflow.com/questions/1171196
      as.character = sql_cast("VARCHAR2(255)"),
      # bit64::as.integer64 can translate to BIGINT for some
      # vendors, which is equivalent to NUMBER(19) in Oracle
      # https://docs.oracle.com/cd/B19306_01/gateways.102/b14270/apa.htm
      as.integer64 = sql_cast("NUMBER(19)"),
      as.numeric = sql_cast("NUMBER"),
      as.double = sql_cast("NUMBER"),
      # https://docs.oracle.com/cd/B19306_01/server.102/b14200/operators003.htm#i997789
      paste = sql_paste_infix(" ", "||", function(x)
        sql_expr(cast(!!x %as% text))),
      paste0 = sql_paste_infix("", "||", function(x)
        sql_expr(cast(!!x %as% text))),

      # lubridate functions https://lubridate.tidyverse.org/reference/index.html
      # Date-time helpers
      now = function()
        build_sql("SYSDATE"),
      today = function()
        build_sql("TRUNC(SYSDATE)"),

      # Setting, getting, and rounding
      year = function(x) {
        build_sql(
          "TO_NUMBER(",
          sql_expr(extract(year %from% !!x)),
          ")"
        )
      },
      `year<-` = function(x, value) {
        build_sql(
          "TO_DATE(LPAD(",
          !!value,
          ",4,'0')||TO_CHAR(",
          !!x,
          ",'DDMMHH24MISS'), 'YYYYDDMMHH24MISS')"
        )
      },
      # isoyear()
      # epiyear()
      quarter = function(x,
                               with_year = FALSE,
                               fiscal_start = 1) {
        if (with_year) {
          build_sql(
            "(TO_NUMBER(",
            sql_expr(extract(year %from% !!x)),
            "|| '.' ||TO_CHAR(ADD_MONTHS(",
            !!x,
            ", 13-",
            fiscal_start,
            "),'Q')))"
          )
        }
        else {
          sql_expr((TO_NUMBER(TO_CHAR(ADD_MONTHS(!!x, 13 - !!fiscal_start), "Q"))))
        }
      },
      semester = function(x, with_year = FALSE) {
        if (with_year) {
          build_sql(
            "(TO_NUMBER(",
            sql_expr(extract(year %from% !!x)),
            "|| '.' ||",
            sql_expr(CEIL(extract(
              month %from% !!x
            ) / 6)),
            "))"
          )
        }
        else {
          sql_expr(CEIL(extract(month %from% !!x) / 6))
        }
      },
      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          sql_expr((TO_NUMBER(extract(month %from% !!x))))
        }
        else if (abbr) {
          build_sql("TO_CHAR(", !!x, ", 'Mon')")
        }
        else {
          warning("Ordered factors do not exist in SQL, data will be character only.")
          build_sql("TRIM(TO_CHAR(", !!x, ", 'Month'))")
        }
      },
      # https://asktom.oracle.com/pls/apex/asktom.search?tag=changing-a-year-within-a-date
      # TODO error: cannot overflow more than 12 months due to the case when type requirements...
      `month<-` = function(x, value) {
        build_sql(
          "(ADD_MONTHS(TO_DATE('01'||TO_CHAR(",
          !!x,
          ",'DDYYYYHH24MISS'), 'MMDDYYYYHH24MISS'),-1+",
          case_when(
            toupper(!!value) %in% c("JAN", "JANUARY", "1", "01", 1) ~ "1",
            toupper(!!value) %in% c("FEB", "FEBRUARY", "2", "02", 2) ~ "2",
            toupper(!!value) %in% c("MAR", "MARCH", "3", "03", 3) ~ "3",
            toupper(!!value) %in% c("APR", "APRIL", "4", "04", 4) ~ "4",
            toupper(!!value) %in% c("MAY", "5", "05", 5) ~ "5",
            toupper(!!value) %in% c("JUN", "JUNE", "6", "06", 6) ~ "6",
            toupper(!!value) %in% c("JUL", "JULY", "7", "07", 7) ~ "7",
            toupper(!!value) %in% c("AUG", "AUGUST", "8", "08", 8) ~ "8",
            toupper(!!value) %in% c("SEP", "SEPTEMBER", "9", "09", 9) ~ "9",
            toupper(!!value) %in% c("OCT", "OCTOBER", "10", 10) ~ "10",
            toupper(!!value) %in% c("NOV", "NOVEMBER", "11", 11) ~ "11",
            toupper(!!value) %in% c("DEC", "DECEMBER", "12", 12) ~ "12",
            TRUE ~ as.character(!!value)
          ),
          "))"
        )
      },
      week = function(x) {
        build_sql("(TO_NUMBER(TO_CHAR(", !!x, ", 'WW')))")
      },
      # `week<-`()
      isoweek = function(x) {
        build_sql("(TO_NUMBER(TO_CHAR(", !!x, ", 'IW')))")
      },
      # epiweek() # needs to use next_date rounding probably
      day = function(x) {
        build_sql("(TO_NUMBER(TO_CHAR(", x, ", 'DD')))")
      },
      mday = function(x) {
        build_sql("(TO_NUMBER(TO_CHAR(", x, ", 'DD')))")
      },
      # TODO in week start support?
      wday = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          build_sql("(MOD(1 + TRUNC(", !!x, ") - TRUNC(", !!x, ", 'IW'),7) +1 )")
        }
        else if (abbr) {
          build_sql("TO_CHAR(", !!x, ", 'DY')")
        }
        else {
          build_sql("TO_CHAR(", !!x, ", 'DAY')")
        }
      },
      # qday()
      yday = function(x) {
        sql_expr((TO_NUMBER(TO_CHAR(!!x, "DDD"))))
      },
      `day<-` = function(x, value) {
        build_sql(
          "(TO_DATE('01'||TO_CHAR(",
          !!x,
          ",'MMYYYYHH24MISS'), 'DDMMYYYYHH24MISS') + ",
          !!value,
          " -1)"
        )
      },
      `mday<-` = function(x, value) {
        build_sql(
          "(TO_DATE('01'||TO_CHAR(",
          !!x,
          ",'MMYYYYHH24MISS'), 'DDMMYYYYHH24MISS') + ",
          !!value,
          " -1)"
        )
      },
      # `qday<-`()
      # `wday<-`()
      `yday<-` = function(x, value) {
        build_sql(
          "(TO_DATE('001'||TO_CHAR(",
          !!x,
          ",'YYYYHH24MISS'), 'DDDYYYYHH24MISS') + ",
          !!value,
          "-1)"
        )
      },
      hour = function(x) {
        build_sql("(TO_NUMBER(TO_CHAR(", x, ", 'HH24')))")
      },
      `hour<-` = function(x, value) {
        build_sql(
          "(TO_DATE(LPAD(MOD(",
          !!value,
          ",24),2,'0')||TO_CHAR(",
          !!x,
          ",'DDMMYYYYMISS'), 'HH24DDMMYYYYMISS') + FLOOR(",
          !!value,
          "/24))"
        )
      },
      minute = function(x) {
        build_sql("(TO_NUMBER(TO_CHAR(", x, ", 'MI')))")
      },
      `minute<-` = function(x, value) {
        build_sql(
          "(TO_DATE(LPAD(MOD(",
          !!value,
          ",60),2,'0')||TO_CHAR(",
          !!x,
          ",'HH24DDMMYYYYSS'), 'MIHH24DDMMYYYYSS') + FLOOR(",
          !!value,
          "/60)/24)"
        )
      },
      second = function(x) {
        build_sql("(TO_NUMBER(TO_CHAR(", x, ", 'SS')))")
      },

      `second<-` = function(x, value) {
        build_sql(
          "(TO_DATE(LPAD(MOD(",
          !!value,
          ",60),2,'0')||TO_CHAR(",
          !!x,
          ",'MIHH24DDMMYYYY'), 'SSMIHH24DDMMYYYY') + FLOOR(",
          !!value,
          "/60)/24/60)"
        )
      },
      # tz() `tz<-`()
      # Other modification functions
      floor_date = function(x,
                                  unit = "second",
                                  week_start = getOption("lubridate.week.start", 7)) {
        if (!length(x)) {
          return(x)
        }
        parsed_unit <- parse_period_unit(unit)
        n <- parsed_unit$n
        unit <- standardise_period_names(parsed_unit$unit)
        switch(
          unit,
          second = {
            if (n > 60) {
              stop("Error: Rounding with with second > 60 is not supported")
            }
            build_sql(
              "(TRUNC(", !!x,
              ", 'mi') + (FLOOR((", !!x,
              "- TRUNC(", !!x,
              ", 'mi')) * 24 * 60 * (60/", !!n,
              "))/ (24* 60 *(60/", !!n,
              "))))"
            )
          },
          minute = {
            if (n > 60) {
              stop("Error: Rounding with with minute > 60 is not supported")
            }
            build_sql(
              "(TRUNC(", !!x,
              ", 'hh') + (FLOOR((", !!x,
              "- TRUNC(", !!x,
              ", 'hh')) * 24 * (60/", !!n,
              "))/ (24*(60/", !!n,
              "))))"
            )
          },
          hour = {
            if (n > 24) {
              stop("Error: Rounding with with second > 24 is not supported")
            }
            build_sql(
              "(TRUNC(", !!x,
              ") + (FLOOR((", !!x,
              "- TRUNC(", !!x,
              ")) * 24/", !!n,
              ")/ (24/", !!n,
              ")))"
            )
          },
          day = {
            if (n > 31) {
              stop("Error: Rounding with with day > 31 is not supported")
            }
            build_sql(
              "(TRUNC(", !!x,
              ", 'mm') + (FLOOR((", !!x,
              "- TRUNC(", !!x,
              ", 'mm')) * 1/", !!n,
              ")/ (1/", !!n,
              ")))"
            )
          },
          week = {
            if (n != 1) {
              warning("Multi-unit not supported for weeks. Ignoring.")
            }
            switch(
              week_start,
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'MON') -7))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'TUE') -7))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'WED') -7))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'THU') -7))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'FRI') -7))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'SAT') -7))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'SUN') -7))"),
              stop("week_start value must be 1-7 (Monday-Sunday)") # does a default value work with integer switch?
            )
          },
          month = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), FLOOR(MONTHS_BETWEEN(", !!x,
              ", TRUNC(", !!x,
              ", 'yyyy'))/", !!n,
              ")*", !!n,
              "))"
            )
          },
          year = {
            build_sql(
              "(
              CASE WHEN ",
              !!n,
              "= 1 then TRUNC(",
              !!x,
              ", 'YYYY')
              ELSE ADD_MONTHS(TRUNC(",
              !!x,
              ", 'YYYY'),  (-MOD(EXTRACT(year from ",
              !!x,
              "), ",
              !!n,
              "))*12) end)"
            )
          },
          bimonth = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), FLOOR(MONTHS_BETWEEN(", !!x,
              ", TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*2))*", !!n,
              "*2))"
            )
          },
          quarter = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), FLOOR(MONTHS_BETWEEN(", !!x,
              ", TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*3))*", !!n,
              "*3))"
            )
          },
          halfyear = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), FLOOR(MONTHS_BETWEEN(", !!x,
              ", TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*6))*", !!n,
              "*6))"
            )
          },
          season = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), -1  + FLOOR(MONTHS_BETWEEN(ADD_MONTHS(", !!x,
              ",1), TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*3))*", !!n,
              "*3))"
            )
          },
          stop("Error: Failed to parse units.")
        )
      },
      ceiling_date = function(x,
                                    unit = "second",
                                    week_start = getOption("lubridate.week.start", 7)) {
        if (!length(x)) {
          return(x)
        }
        parsed_unit <- parse_period_unit(unit)
        n <- parsed_unit$n
        unit <- standardise_period_names(parsed_unit$unit)
        switch(
          unit,
          # second is more complicated due to the fact that TRUNC will implicetely convert timestamps to date
          # and date does not have milliseconds
          # TODO make sure this works for non-interger seconds
          second = {
            build_sql(
              "(TRUNC(", !!x,
              ", 'mi') + (CEIL((", !!x,
              "- TRUNC(", !!x,
              ", 'mi')) * 24 * 60 * (60/", !!n,
              "))/ (24 * 60 * (60/", !!n,
              "))))"
            )
          },
          minute = {
            build_sql(
              "(TRUNC(", !!x,
              ", 'hh') + (CEIL((", !!x,
              "- TRUNC(", !!x,
              ", 'hh')) * 24 * (60/", !!n,
              "))/ (24*(60/", !!n,
              "))))"
            )
          },
          hour = {
            build_sql(
              "(TRUNC(", !!x,
              ") + (CEIL((", !!x,
              "- TRUNC(", !!x,
              ")) * 24 /", !!n,
              ")/ (24/", !!n,
              ")))"
            )
          },
          day = {
            if (n > 31) {
              stop("Error: Rounding with with day > 31 is not supported")
            }
            build_sql(
              "(TRUNC(", !!x,
              ", 'mm') + (CEIL((", !!x,
              "- TRUNC(", !!x,
              ", 'mm')) * 1/", !!n,
              ")/ (1/", !!n,
              ")))"
            )
          },
          week = {
            if (n != 1) {
              warning("Multi-unit not supported for weeks. Ignoring.")
            }
            switch(
              week_start,
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'MON')))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'TUE')))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'WED')))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'THU')))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'FRI')))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'SAT')))"),
              build_sql("(TRUNC(NEXT_DAY(", !!x, ",'SUN')))"),
              stop("week_start value must be 1-7 (Monday-Sunday)")
            )
          },
          month = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), CEIL(MONTHS_BETWEEN(TRUNC(", !!x,
              ", 'mm')+1, TRUNC(", !!x,
              ", 'yyyy'))/", !!n,
              ")*", !!n,
              "))"
            )
          },
          year = {
            build_sql(
              "(
              CASE WHEN ",
              !!n,
              "= 1 then ADD_MONTHS(TRUNC(",
              !!x,
              ", 'YYYY'), 12)
              ELSE ADD_MONTHS(TRUNC(",
              !!x,
              ", 'YYYY'),  (", !!n, "-MOD(EXTRACT(year from ",
              !!x,
              "), ",
              !!n,
              "))*12) end)"
            )
          },
          # TODO these should be refactored into calls to month with multiplied n (except season).
          bimonth = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), CEIL(MONTHS_BETWEEN(TRUNC(", !!x,
              ", 'mm')+1, TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*2))*", !!n,
              "*2))"
            )
          },
          quarter = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), CEIL(MONTHS_BETWEEN(TRUNC(", !!x,
              ", 'mm')+1, TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*3))*", !!n,
              "*3))"
            )
          },
          halfyear = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), CEIL(MONTHS_BETWEEN(TRUNC(", !!x,
              ", 'mm')+1, TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*6))*", !!n,
              "*6))"
            )
          },
          season = {
            build_sql(
              "(ADD_MONTHS(TRUNC(", !!x,
              ", 'yyyy'), -1  + CEIL(MONTHS_BETWEEN(ADD_MONTHS(TRUNC(", !!x,
              ", 'mm')+1,1), TRUNC(", !!x,
              ", 'yyyy'))/(", !!n,
              "*3))*", !!n,
              "*3))"
            )
          },
          stop("Error: Failed to parse units.")
        )
      },
      # This does not work, how should I call the function I just defined?
      # round_date = function(x,
      #                             unit = "second",
      #                             week_start = getOption("lubridate.week.start", 7)) {
      #   build_sql(
      #     "(CASE WHEN ",
      #     ceiling_date(x, unit, week_start),
      #     " - ",
      #     !!x,
      #     " > ",
      #     !!x,
      #     " - ",
      #     floor_date(x, unit, week_start),
      #     " THEN ",
      #     floor_date(x, unit, week_start),
      #     " ELSE ",
      #     ceiling_date(x, unit, week_start),
      #     ")"
      #   )
      # },
      # Other date-time components
      # No boolean type in oracle SQL https://community.oracle.com/thread/2473001?start=0&tstart=0
      days_in_month = function(x)
        build_sql("(TO_CHAR(ADD_MONTHS(TRUNC(", x, ", 'mm'), 1)-1,'DD'))"),
      leap_year = function(x) {
        warning("Oracle SQL does not support boolean types, return will be 'TRUE' or 'FALSE' strings")
        build_sql(
          "CASE WHEN REMAINDER(EXTRACT( year from ",
          !!x,
          "), 4) = 0 AND (
          REMAINDER(EXTRACT( year from ",
          !!x,
          "), 100) != 0 or REMAINDER(EXTRACT( year from ",
          !!x,
          "), 400) = 0)
          THEN 'TRUE' ELSE 'FALSE' END"
        )
      }
    ),
    base_odbc_agg,
    base_odbc_win
  )
}

#' @export
db_explain.Oracle <- function(con, sql, ...) {
  DBI::dbExecute(con, build_sql("EXPLAIN PLAN FOR ", sql, con = con))
  expl <-
    DBI::dbGetQuery(
      con,
      "SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY())"
    )
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

#' @export
db_analyze.Oracle <- function(con, table, ...) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_4005.htm
  sql <- dbplyr::build_sql("ANALYZE TABLE ", as.sql(table), " COMPUTE STATISTICS",
    con = con
  )
  DBI::dbExecute(con, sql)
}

#' @export
sql_subquery.Oracle <-
  function(con, from, name = unique_name(), ...) {
    # Table aliases in Oracle should not have an "AS": https://www.techonthenet.com/oracle/alias.php
    if (is.ident(from)) {
      build_sql("(", from, ") ", if (!is.null(name)) {
        ident(name)
      }, con = con)
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
      "EXECUTE IMMEDIATE 'DROP TABLE ",
      ident(table),
      "';",
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
