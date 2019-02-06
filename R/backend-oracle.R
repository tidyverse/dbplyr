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
    c("select",
      "from",
      "where",
      "group_by",
      "having",
      "order_by",
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
                     con = con)
  } else{
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
        if (is.na(format) | format == '%F' | format == '%Y-%m-%d') {
          build_sql(sql("DATE "), x)
        }
        else if (grepl(x, '%C') | grepl(x, '%e') | grepl(x, '%w') |
                 grepl(x, '%W') | grepl(x, '%U') | grepl(x, '%x') |
                 grepl(x, '%y') | grepl(x, '%z') | grepl(x, '%D')) {
          stop(
            "Error: One or more of the format identifiers is not curerntly supported by dbplyr for Oracle SQL"
          )
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


      #Setting, getting, and rounding
      year = function(x) {
        build_sql(
          "to_number(",
          sql_expr(extract(year %from% !!x)),
          ")"
          )
        },
      `year<-` = function(x, value) {
        build_sql(
          "to_date(lpad(",
          !!value,
          ",4,'0')||to_char(",
          !!x,
          ",'DDMMHH24MISS'), 'YYYYDDMMHH24MISS')"
        )
      },
      #isoyear()
      #epiyear()
      quarter = function(x,
                         with_year = FALSE,
                         fiscal_start = 1) {
        if (with_year) {
          build_sql(
            "(to_number(",
            sql_expr(extract(year %from% !!x)),
            "|| '.' ||to_char(add_months(",
            x ,
            ", 13-",
            fiscal_start,
            "),'Q')))"
          )
        }
        else{
          sql_expr((to_number(to_char(add_months(!!x, 13-!!fiscal_start), 'Q'))))
        }
      },
      semester = function(x, with_year = FALSE) {
        if (with_year) {
          build_sql("(to_number(",
                    sql_expr(extract(year %from% !!x)),
                    "|| '.' ||",
                    sql_expr(ceil(extract(
                      month %from% !!x
                    ) / 6)),
                    "))")
        }
        else{
          sql_expr(ceil(extract(month %from% !!x) / 6))
        }
      },
      month = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          sql_expr((to_number(extract(month %from% !!x))))
        }
        else if (abbr) {
          build_sql("TO_CHAR(", x, ", 'Mon')")
        }
        else {
          build_sql("TO_CHAR(", x, ", 'Month')")
        }
      },
      # https://asktom.oracle.com/pls/apex/asktom.search?tag=changing-a-year-within-a-date
      # TODO error: cannot overflow more than 12 months due to the case when type requirements...
      `month<-` = function(x, value) {
        build_sql(
          "(add_months(to_date('01'||to_char(",
          !!x,
          ",'DDYYYYHH24MISS'), 'MMDDYYYYHH24MISS'),-1+",
          case_when(
            toupper(!!value) %in% c('JAN', 'JANUARY', '1', '01', 1) ~ '1',
            toupper(!!value) %in% c('FEB', 'FEBRUARY', '2', '02', 2) ~ '2',
            toupper(!!value) %in% c('MAR', 'MARCH', '3', '03', 3) ~ '3',
            toupper(!!value) %in% c('APR', 'APRIL', '4', '04', 4) ~ '4',
            toupper(!!value) %in% c('MAY', '5', '05', 5) ~ '5',
            toupper(!!value) %in% c('JUN', 'JUNE', '6', '06', 6) ~ '6',
            toupper(!!value) %in% c('JUL', 'JULY', '7', '07', 7) ~ '7',
            toupper(!!value) %in% c('AUG', 'AUGUST', '8', '08', 8) ~ '8',
            toupper(!!value) %in% c('SEP', 'SEPTEMBER', '9', '09', 9) ~ '9',
            toupper(!!value) %in% c('OCT', 'OCTOBER', '10', 10) ~ '10',
            toupper(!!value) %in% c('NOV', 'NOVEMBER', '11', 11) ~ '11',
            toupper(!!value) %in% c('DEC', 'DECEMBER', '12', 12) ~ '12',
            TRUE ~ as.character(!!value)
          ),
          "))"
        )
      },
      week = function(x) {
        build_sql("(to_number(TO_CHAR(", x, ", 'WW')))")
      },
      #`week<-`()
      isoweek = function(x) {
        build_sql("(to_number(TO_CHAR(", x, ", 'IW')))")
      },
      #epiweek() # needs to use next_date rounding probably
      day = function(x) {
        build_sql("(to_number(TO_CHAR(", x, ", 'DD')))")
      },
      mday = function(x) {
        build_sql("(to_number(TO_CHAR(", x, ", 'DD')))")
      },
      #TODO in week start support?
      wday = function(x, label = FALSE, abbr = TRUE) {
        if (!label) {
          build_sql("(to_number(TO_CHAR(", x, ", 'D')))")
        }
        else if (abbr) {
          build_sql("TO_CHAR(", x, ", 'DY')")
        }
        else {
          build_sql("TO_CHAR(", x, ", 'DAY')")
        }
      },

      #qday()

      yday = function(x) {
        sql_expr((to_number(to_char(!!x, 'DDD'))))
      },
      `day<-` = function(x, value) {
        build_sql(
          "(to_date('01'||to_char(",
          !!x,
          ",'MMYYYYHH24MISS'), 'DDMMYYYYHH24MISS') + ",
          !!value,
          " -1)"
        )
      },
      `mday<-` = function(x, value) {
        build_sql(
          "(to_date('01'||to_char(",
          !!x,
          ",'MMYYYYHH24MISS'), 'DDMMYYYYHH24MISS') + ",
          !!value,
          " -1)"
        )
      },
      #`qday<-`() `wday<-`()
      `yday<-` = function(x, value) {
        build_sql(
          "(to_date('001'||to_char(",
          !!x,
          ",'YYYYHH24MISS'), 'DDDYYYYHH24MISS') + ",
          !!value,
          "-1)"
        )
      },
      hour = function(x) {
        build_sql("(to_number(TO_CHAR(", x, ", 'HH24')))")
      },
      `hour<-` = function(x, value) {
        build_sql(
          "(to_date(lpad(mod(",
          !!value,
          ",24),2,'0')||to_char(",
          !!x,
          ",'DDMMYYYYMISS'), 'HH24DDMMYYYYMISS') + floor(",
          !!value,
          "/24))"
        )
      },
      minute = function(x) {
        build_sql("(to_number(to_char(", x , ", 'MI')))")
      },
      `minute<-` = function(x, value) {
        build_sql(
          "(to_date(lpad(mod(",
          !!value,
          ",60),2,'0')||to_char(",
          !!x,
          ",'HH24DDMMYYYYSS'), 'MIHH24DDMMYYYYSS') + floor(",
          !!value,
          "/60)/24)"
        )
      },
      second = function(x) {
        build_sql("(to_number(to_char(", x, ", 'SS')))")
      },

      `second<-` = function(x, value) {
        build_sql(
          "(to_date(lpad(mod(",
          !!value,
          ",60),2,'0')||to_char(",
          !!x,
          ",'MIHH24DDMMYYYY'), 'SSMIHH24DDMMYYYY') + floor(",
          !!value,
          "/60)/24/60)"
        )
      },

      #tz() `tz<-`()

      # Other modification functions
      round_date = function(x,
                            unit = "second",
                            week_start = getOption("lubridate.week.start", 7)) {
        if (!length(x))
          return(x)
        parsed_unit <- parse_period_unit(unit)
        n <- parsed_unit$n
        unit <- standardise_period_names(parsed_unit$unit)
        switch(
          unit,
          # second is more complicated due to the fact that trunc will implicetely convert timestamps to date
          # and date does not have milliseconds
          # TODO test seconds round work
          second = {
            if (n > 60)
              stop('Error: Rounding with with second > 60 is not supported')
            build_sql(
              "(trunc(",!!x,
              ", 'mi') +
              round(EXTRACT(second FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24 * 60 * 60))"
              )
          },
          # TODO test minutes work
          minute = {
            if (n > 60)
              stop('Error: Rounding with with minute > 60 is not supported')
            build_sql(
              "(trunc(",!!x,
              ", 'HH24') +
              round(EXTRACT(minute FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24 * 60))"
              )
          },
          # TODO test hours work
          hour = {
            if (n > 24)
              stop('Error: Rounding with with second > 24 is not supported')
            build_sql(
              "(trunc(",!!x,
              ", 'DD') +
              floor(EXTRACT(hour FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24))"
              )
          },
          # TODO test day work
          day = {
            if (n > 31)
              stop('Error: Rounding with with day > 31 is not supported')
            build_sql(
              "(
              case when ",
              !!n,
              "= 1 then trunc(",
              !!x,
              ", 'DD')
              else trunc(",
              !!x,
              ", 'MM') + floor(EXTRACT(day FROM ",
              !!x,
              ")/",
              n,
              "-1)*",
              n,
              "end)"
              )
          },
          # TODO make week work
      #    week = {
      #      if (n != 1)
      #        warning('Multi-unit not supported for weeks. Ignoring.')
      #      switch(week_start,
      #             #
      #             #
      #             #
      #             #
      #             stop('week_start value must be 1-7 (Monday-Sunday)') #does a default value work with integer switch?)
      #    },
          # TODO test month work
          month = {
            build_sql(
              "(
              add_months(trunc(",!!x,
              ", 'YYYY'), floor(EXTRACT(month FROM add_months(round(",!!x,
              ", 'mm'),-1))/",
              n,
              ")*",
              n,
              "))"
              )
          },
          # TODO test year work
          year = {
            build_sql(
              "(
              case when ",
              !!n,
              "= 1 then trunc(",
              !!x,
              ", 'YYYY')
              else add_months(round(",
              !!x,
              ", 'YYYY'),  (-remainder(extract(year from ",
              !!x,
              "), ",
              !!n,
              "))*12) end)"
              )
          },
          # TODO test bimonth work
          bimonth = {
            build_sql(
              "(
              add_months(trunc(",!!x,
              ", 'YYYY'), floor(EXTRACT(month FROM add_months(round(",!!x,
              ", 'mm'),-1))/(",
              n,
              "*2)*(",
              n,
              "*2))))"
              )
          },
          # TODO test quarter work
          quarter = {
            build_sql(
              "(
              add_months(trunc(",!!x,
              ", 'YYYY'), floor(EXTRACT(month FROM add_months(round(",!!x,
              ", 'mm'),-1))/(",
              n,
              "*3)*(",
              n,
              "*3))))"
              )
          },
          # TODO test halfyear work
          halfyear = {
            build_sql(
              "(
              add_months(trunc(",!!x,
              ", 'YYYY'), floor(EXTRACT(month FROM add_months(round(",!!x,
              ", 'mm'),-1))/(",
              n,
              "*6)*(",
              n,
              "*6))))"
              )
          },
          # TODO test season work
          season = {
            build_sql(
              "(
              add_months(trunc(",!!x,
              ", 'YYYY'), -1 + floor(EXTRACT(month FROM add_months(round(",!!x,
              ", 'mm'),-1))/(",
              n,
              "*3)*(",
              n,
              "*3))))"
              )
          },
          stop("Error: Failed to parse units.")
              )
      },
      floor_date = function(x,
                            unit = "second",
                            week_start = getOption("lubridate.week.start", 7)) {
        if (!length(x))
          return(x)
        parsed_unit <- parse_period_unit(unit)
        n <- parsed_unit$n
        unit <- standardise_period_names(parsed_unit$unit)
        switch(
          unit,
          # second is more complicated due to the fact that trunc will implicetely convert timestamps to date
          # and date does not have milliseconds
          # TODO make sure this works for non-interger seconds
          second = {
            if (n > 60)
              stop('Error: Rounding with with second > 60 is not supported')
            build_sql(
              "(trunc(",!!x,
              ", 'mi') +
              floor(EXTRACT(second FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24 * 60 * 60))"
              )
          },
          minute = {
            if (n > 60)
              stop('Error: Rounding with with minute > 60 is not supported')
            build_sql(
              "(trunc(",!!x,
              ", 'HH24') +
              floor(EXTRACT(minute FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24 * 60))"
              )
          },
          hour = {
            if (n > 24)
              stop('Error: Rounding with with second > 24 is not supported')
            build_sql(
              "(trunc(",!!x,
              ", 'DD') +
              floor(EXTRACT(hour FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24))"
              )
          },
          day = {
            if (n > 31)
              stop('Error: Rounding with with day > 31 is not supported')
            build_sql(
              "(
              case when ",
              !!n,
              "= 1 then trunc(",
              !!x,
              ", 'DD')
              else trunc(",
              !!x,
              ", 'MM') + floor(EXTRACT(day FROM ",
              !!x,
              "-1)/",
              n,
              ")*",
              n,
              "end)"
              )
          },
          week = {
            if (n != 1)
              warning('Multi-unit not supported for weeks. Ignoring.')
            switch(
              week_start,
              build_sql("(trunc(next_day(", !!x, ",'MON') -7))"),
              build_sql("(trunc(next_day(", !!x, ",'TUE') -7))"),
              build_sql("(trunc(next_day(", !!x, ",'WED') -7))"),
              build_sql("(trunc(next_day(", !!x, ",'THU') -7))"),
              build_sql("(trunc(next_day(", !!x, ",'FRI') -7))"),
              build_sql("(trunc(next_day(", !!x, ",'SAT') -7))"),
              build_sql("(trunc(next_day(", !!x, ",'SUN') -7))"),
              stop('week_start value must be 1-7 (Monday-Sunday)') #does a default value work with integer switch?
            )
          },
          month = {
            if (n < 12) {
              build_sql(
                "(
                case when ",
                !!n,
                "= 1 then trunc(",
                !!x,
                ", 'MM')
                else add_months(trunc(",
                !!x,
                ", 'YYYY'), floor(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/",
                n,
                ")*",
                n,
                ")end)"
                )
            }
            else{
              build_sql("(trunc(", !!x, ", 'YYYY'))")
            }
          },
          year = {
            build_sql(
              "(
              case when ",
              !!n,
              "= 1 then trunc(",
              !!x,
              ", 'YYYY')
              else add_months(trunc(",
              !!x,
              ", 'YYYY'),  (-mod(extract(year from ",
              !!x,
              "), ",
              !!n,
              "))*12) end)"
              )
          },
          bimonth = {
            if (n < 6) {
              build_sql(
                "(add_months(trunc(",
                !!x,
                ", 'YYYY'), floor(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*2))*(",
                n,
                "*2)))"
              )
            }
            else{
              build_sql("(trunc(", !!x, ", 'yyyy'))")
            }
          },
          quarter = {
            if (n < 4) {
              build_sql(
                "(add_months(trunc(",
                !!x,
                ", 'YYYY'), floor(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*3))*(",
                n,
                "*3)))"
              )
            }
            else{
              build_sql("(trunc(", !!x, ", 'yyyy'))")
            }
          },
          halfyear = {
            if (n < 2) {
              build_sql(
                "(add_months(trunc(",
                !!x,
                ", 'YYYY'), floor(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*6))*(",
                n,
                "*6)))"
              )
            }
            else{
              build_sql("(trunc(", !!x, ", 'yyyy'))")
            }
          },
          season = {
            if (n < 4) {
              build_sql(
                "(add_months(trunc(",
                !!x,
                ", 'YYYY'), (-1) + floor(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*3))*(",
                n,
                "*3)))"
              )
            }
            else{
              build_sql("(add_months(trunc(", !!x, ", 'yyyy'), -1))")
            }
          },
          stop("Error: Failed to parse units.")
              )
      },
      ceiling_date = function(x,
                              unit = "second",
                              week_start = getOption("lubridate.week.start", 7)) {
        if (!length(x))
          return(x)
        parsed_unit <- parse_period_unit(unit)
        n <- parsed_unit$n
        unit <- standardise_period_names(parsed_unit$unit)
        switch(
          unit,
          # second is more complicated due to the fact that trunc will implicetely convert timestamps to date
          # and date does not have milliseconds
          # TODO make sure this works for non-interger seconds
          second = {
            build_sql(
              "(trunc(",!!x,
              ", 'mi') +
              ceil(EXTRACT(second FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24 * 60 * 60))"
              )
          },
          minute = {
            build_sql(
              "(trunc(",!!x,
              ", 'HH24') +
              ceil(EXTRACT(minute FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24 * 60))"
              )
          },
          hour = {
            build_sql(
              "(trunc(",!!x,
              ", 'DD') +
              ceil(EXTRACT(hour FROM cast(",!!x,
              " as timestamp))/",
              n,
              ")*",
              n,
              "/ (24))"
              )
          },
          day = {
            if (n > 31)
              stop('Error: Rounding with with day > 31 is not supported')
            build_sql(
              "(
              case when ",
              !!n,
              "= 1 then trunc(",
              !!x,
              ", 'DD') +1
              else trunc(",
              !!x,
              ", 'MM') + ceil(EXTRACT(day FROM ",
              !!x,
              "-1)/",
              n,
              ")*",
              n,
              "end)"
              )
          },
          week = {
            if (n != 1)
              warning('Multi-unit not supported for weeks. Ignoring.')
            switch(
              week_start,
              build_sql("(trunc(next_day(", !!x, ",'MON')))"),
              build_sql("(trunc(next_day(", !!x, ",'TUE')))"),
              build_sql("(trunc(next_day(", !!x, ",'WED')))"),
              build_sql("(trunc(next_day(", !!x, ",'THU')))"),
              build_sql("(trunc(next_day(", !!x, ",'FRI')))"),
              build_sql("(trunc(next_day(", !!x, ",'SAT')))"),
              build_sql("(trunc(next_day(", !!x, ",'SUN')))"),
              stop('week_start value must be 1-7 (Monday-Sunday)')
            )
          },
          month = {
            if (n < 12) {
              build_sql(
                "(
                case when ",
                !!n,
                "= 1 then add_months(trunc(",
                !!x,
                ", 'MM'), 1)
                else add_months(trunc(",
                !!x,
                ", 'YYYY'), ceil(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/",
                n,
                ")*",
                n,
                ")end)"
                )
            }
            else{
              build_sql("(add_months(trunc(", !!x, ", 'yyyy'), ", n, "))")
            }
          },
          year = {
            # TODO
          },
          # TODO check all these below actually work for n =1 and n >1
          bimonth = {
            if (n < 6) {
              build_sql(
                "add_months(trunc(",
                !!x,
                ", 'YYYY'), ceil(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*2))*(",
                n,
                "*2))"
              )
            }
            else{
              build_sql("(add_months(trunc(", !!x, ", 'yyyy'), ", n, "))")
            }
          },
          quarter = {
            if (n < 4) {
              build_sql(
                "add_months(trunc(",
                !!x,
                ", 'YYYY'), ceil(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*3))*(",
                n,
                "*3))"
              )
            }
            else{
              build_sql("(add_months(trunc(", !!x, ", 'yyyy'), ", n, "))")
            }
          },
          halfyear = {
            if (n < 2) {
              build_sql(
                "add_months(trunc(",
                !!x,
                ", 'YYYY'), ceil(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*6))*(",
                n,
                "*6))"
              )
            }
            else{
              build_sql("(add_months(trunc(", !!x, ", 'yyyy'), ", n, "))")
            }
          },
          season = {
            if (n < 4) {
              build_sql(
                "add_months(trunc(",
                !!x,
                ", 'YYYY'), -1 +  ceil(EXTRACT(month FROM add_months(",
                !!x,
                ",-1))/(",
                n,
                "*3))*(",
                n,
                "*3))"
              )
            }
            else{
              build_sql("(add_months(trunc(", !!x, ", 'yyyy'), ", n, "))")
            }
          },
          stop("Error: Failed to parse units.")
                )
      },
      # Other date-time components
      #TODO make sure all these work
      # No boolean type in oracle SQL https://community.oracle.com/thread/2473001?start=0&tstart=0
      days_in_month = function(x)
        build_sql("(to_char(add_months(trunc(", x, ", 'mm'), 1)-1,'DD'))"),
      leap_year = function(x) {
        warning("Oracle SQL does not support boolean types, return will be 'TRUE' or 'FALSE' strings")
        build_sql(
          "CASE WHEN REMAINDER(extract( year from ",
          !!x,
          "), 4) = 0 AND (
          REMAINDER(extract( year from ",
          !!x,
          "), 100) != 0 or REMAINDER(extract( year from ",
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
    DBI::dbGetQuery(con,
                    "SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY())")
  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}

#' @export
db_analyze.Oracle <- function(con, table, ...) {
  # https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_4005.htm
  sql <- dbplyr::build_sql("ANALYZE TABLE ", as.sql(table), " COMPUTE STATISTICS",
                           con = con)
  DBI::dbExecute(con, sql)
}

#' @export
sql_subquery.Oracle <-
  function(con, from, name = unique_name(), ...) {
    # Table aliases in Oracle should not have an "AS": https://www.techonthenet.com/oracle/alias.php
    if (is.ident(from)) {
      build_sql("(", from, ") ", if (!is.null(name))
        ident(name), con = con)
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
