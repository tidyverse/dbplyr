# ignore nulls
# * hive: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+WindowingAndAnalytics
# * impala: https://docs.cloudera.com/documentation/enterprise/5-11-x/topics/impala_analytic_functions.html
# * mssql: https://docs.microsoft.com/en-us/sql/t-sql/functions/first-value-transact-sql?view=sql-server-ver15
# * oracle: https://oracle-base.com/articles/misc/first-value-and-last-value-analytic-functions
# * redshift: https://docs.aws.amazon.com/redshift/latest/dg/r_WF_first_value.html
# * teradata: https://docs.teradata.com/r/756LNiPSFdY~4JcCCcR5Cw/V~t1FC7orR6KCff~6EUeDQ
#
# no ignore nulls
# * access: https://www.techonthenet.com/access/functions/
# * hana: https://help.sap.com/viewer/4fe29514fd584807ac9f2a04f6754767/2.0.04/en-US/e7ef7cc478f14a408e1af27fc1a78624.html
# * mysql: https://dev.mysql.com/doc/refman/8.0/en/window-function-descriptions.html

#' Fill in missing values with previous or next value
#'
#' @inheritParams arrange.tbl_lazy
#' @param ... <[`tidy-select`][tidyr_tidy_select]> Columns to fill.
#' @param order_by Columns to order by before filling the values.
#' @param .direction Direction in which to fill missing values. Currently
#'   either "down" (the default) or "up".
#'
#' @export
#'
#' @examples
#' squirrels <- tibble::tribble(
#'   ~group,    ~name,     ~role,     ~n_squirrels, ~ n_squirrels2,
#'   1,      "Sam",    "Observer",   NA,                 1,
#'   1,     "Mara", "Scorekeeper",    8,                NA,
#'   1,    "Jesse",    "Observer",   NA,                NA,
#'   1,      "Tom",    "Observer",   NA,                 4,
#'   2,     "Mike",    "Observer",   NA,                NA,
#'   2,  "Rachael",    "Observer",   NA,                 6,
#'   2,  "Sydekea", "Scorekeeper",   14,                NA,
#'   2, "Gabriela",    "Observer",   NA,                NA,
#'   3,  "Derrick",    "Observer",   NA,                NA,
#'   3,     "Kara", "Scorekeeper",    9,                 10,
#'   3,    "Emily",    "Observer",   NA,                NA,
#'   3, "Danielle",    "Observer",   NA,                NA
#' )
#' squirrels$id <- 1:12
#'
#' dbplyr_fill(
#'   tbl_memdb(squirrels),
#'   n_squirrels,
#'   n_squirrels2,
#'   order_by = c(id)
#' )
dbplyr_fill <- function(.data, ..., order_by, .direction = c("down", "up")) {
  # Sources
  # * https://www.xspdf.com/resolution/51601218.html
  # * https://dzone.com/articles/how-to-fill-sparse-data-with-the-previous-non-empt

  sim_data <- simulate_vars(.data)
  cols_to_fill <- syms(names(tidyselect::eval_select(expr(c(...)), sim_data)))
  order_by_cols <- enquo(order_by)
  .direction <- arg_match0(.direction, c("down", "up"))

  dbplyr_fill_int(
    .con = remote_con(.data),
    .data = .data,
    cols_to_fill = cols_to_fill,
    order_by_cols = order_by_cols,
    .direction = .direction
  )
}

dbplyr_fill_int <- function(.con, .data, cols_to_fill, order_by_cols, .direction) {
  UseMethod("dbplyr_fill_int")
}

#' @export
dbplyr_fill_int.SQLiteConnection <- function(.con,
                                             .data,
                                             cols_to_fill,
                                             order_by_cols,
                                             .direction) {
  if (.direction == "up") {
    order_by_cols <- quo(-!!order_by_cols)
  }

  partition_quos <- purrr::map(
    cols_to_fill,
    ~ quo(cumsum(ifelse(is.na(!!.x), 0L, 1L)))
  ) %>%
    set_names(paste0("..dbplyr_partion_", seq_along(cols_to_fill)))

  dp <- .data %>%
    window_order(order_by_cols) %>%
    mutate(!!!partition_quos)

  fill_sql <- purrr::map2(
    cols_to_fill, names(partition_quos),
    ~ translate_sql(
      max(!!.x, na.rm = TRUE),
      con = .con,
      vars_order = translate_sql(!!order_by_cols, con = .con),
      vars_group = c(op_grps(.data), .y),
    )
  ) %>%
    set_names(purrr::map_chr(cols_to_fill, as_name))

  dp %>%
    transmute(
      !!!syms(colnames(.data)),
      !!!fill_sql
    ) %>%
    select(!!!colnames(.data))
}

#' @export
dbplyr_fill_int.PostgreSQL <- dbplyr_fill_int.SQLiteConnection
#' @export
dbplyr_fill_int.PqConnection <- dbplyr_fill_int.SQLiteConnection

#' @export
dbplyr_fill_int.HDB <- dbplyr_fill_int.SQLiteConnection

#' @export
dbplyr_fill_int.ACCESS <- dbplyr_fill_int.SQLiteConnection

#' @export
dbplyr_fill_int.MariaDBConnection <- dbplyr_fill_int.SQLiteConnection
#' @export
dbplyr_fill_int.MySQLConnection <- dbplyr_fill_int.SQLiteConnection
#' @export
dbplyr_fill_int.MySQL <- dbplyr_fill_int.SQLiteConnection

#' @export
dbplyr_fill_int.DBIConnection <- function(.con,
                                          .data,
                                          cols_to_fill,
                                          order_by_cols,
                                          .direction) {
  if (.direction == "up") {
    order_by_cols <- quo(-!!order_by_cols)
  }

  grps <- op_grps(.data)

  fill_sql <- purrr::map(
    cols_to_fill,
    ~ win_over(
      last_value_sql(.con, .x),
      partition = if (!is_empty(grps)) escape(ident(op_grps(.data)), con = .con),
      # TODO is order necessary even if we arrange later?
      order = translate_sql(!!order_by_cols, con = .con),
      con = .con
    )
  ) %>%
    set_names(as.character(cols_to_fill))

  .data %>%
    transmute(
      !!!syms(colnames(.data)),
      !!!fill_sql
    )
}

last_value_sql <- function(con, x) {
  UseMethod("last_value_sql")
}

#' @export
last_value_sql.DBIConnection <- function(con, x) {
  build_sql("LAST_VALUE(", ident(as.character(x)), " IGNORE NULLS)", con = con)
}

#' @export
last_value_sql.Hive <- function(con, x) {
  translate_sql(last_value(!!x, TRUE), con = con)
}
