#' Fill in missing values with previous or next value
#'
#' @inheritParams arrange.tbl_lazy
#' @param ... Columns to fill.
#' @param .direction Direction in which to fill missing values. Currently
#'   either "down" (the default) or "up". Note that "up" does not work when
#'   `.data` is sorted by non-numeric columns. As a workaround revert the order
#'   yourself beforehand; for example replace `arrange(x, desc(y))` by
#'   `arrange(desc(x), y)`.
#'
#' @examplesIf rlang::is_installed("tidyr", version = "1.0.0")
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
#' tbl_memdb(squirrels) %>%
#'   window_order(id) %>%
#'   tidyr::fill(
#'     n_squirrels,
#'     n_squirrels2,
#'   )
fill.tbl_lazy <- function(.data, ..., .direction = c("down", "up", "updown", "downup")) {
  cols_to_fill <- tidyselect::eval_select(expr(c(...)), .data)
  cols_to_fill <- syms(names(cols_to_fill))
  order_by_cols <- op_sort(.data)
  .direction <- arg_match0(.direction, c("down", "up", "updown", "downup"))

  if (is_empty(order_by_cols)) {
    cli_abort(
      c(
        x = "{.arg .data} does not have explicit order.",
        i = "Please use {.fun dbplyr::window_order} to make order explicit."
      )
    )
  }

  if (.direction == "updown") {
    .data <- tidyr::fill(.data, !!!cols_to_fill, .direction = "up")
    .data <- tidyr::fill(.data, !!!cols_to_fill, .direction = "down")
    return(.data)
  } else if (.direction == "downup") {
    .data <- tidyr::fill(.data, !!!cols_to_fill, .direction = "down")
    .data <- tidyr::fill(.data, !!!cols_to_fill, .direction = "up")
    return(.data)
  }

  if (.direction == "up") {
    order_by_cols <- purrr::map(order_by_cols, swap_order_direction)
  }

  dbplyr_fill0(
    .con = remote_con(.data),
    .data = .data,
    cols_to_fill = cols_to_fill,
    order_by_cols = order_by_cols,
    .direction = .direction
  )
}

dbplyr_fill0 <- function(.con, .data, cols_to_fill, order_by_cols, .direction) {
  UseMethod("dbplyr_fill0")
}

# databases with support for `IGNORE NULLS`
# * hive: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+WindowingAndAnalytics
# * impala: https://docs.cloudera.com/documentation/enterprise/5-11-x/topics/impala_analytic_functions.html
# * oracle: https://oracle-base.com/articles/misc/first-value-and-last-value-analytic-functions
# * redshift: https://docs.aws.amazon.com/redshift/latest/dg/r_WF_first_value.html
# * teradata: https://docs.teradata.com/r/756LNiPSFdY~4JcCCcR5Cw/V~t1FC7orR6KCff~6EUeDQ
#' @export
dbplyr_fill0.DBIConnection <- function(.con,
                                       .data,
                                       cols_to_fill,
                                       order_by_cols,
                                       .direction) {
  # strategy:
  # 1. construct a window
  # * from the first row to the current row
  # * ordered by `order_by_cols`
  # * partitioned by groups if any
  # 2. in this window use the last value ignoring nulls; in SQL this is (usually)
  #    `LAST_VALUE(<column> IGNORE NULLS)`
  #
  # Source: https://dzone.com/articles/how-to-fill-sparse-data-with-the-previous-non-empt
  grps <- op_grps(.data)

  fill_sql <- purrr::map(
    cols_to_fill,
    ~ translate_sql(
      last(!!.x, na_rm = TRUE),
      vars_group = op_grps(.data),
      vars_order = translate_sql(!!!order_by_cols, con = .con),
      vars_frame = c(-Inf, 0),
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

# databases without support for `IGNORE NULLS`
# * sqlite
# * access: https://www.techonthenet.com/access/functions/
# * hana: https://help.sap.com/viewer/4fe29514fd584807ac9f2a04f6754767/2.0.04/en-US/e7ef7cc478f14a408e1af27fc1a78624.html
# * mysql: https://dev.mysql.com/doc/refman/8.0/en/window-function-descriptions.html
# * mariadb
# * postgres: https://www.postgresql.org/docs/13/functions-window.html
# * mssql: https://docs.microsoft.com/en-us/sql/t-sql/functions/first-value-transact-sql?view=sql-server-ver15
#   -> `IGNORE NULLS` only in Azure SQL Edge
#' @export
dbplyr_fill0.SQLiteConnection <- function(.con,
                                          .data,
                                          cols_to_fill,
                                          order_by_cols,
                                          .direction) {
  # this strategy is used for databases that don't support `IGNORE NULLS` in
  # `LAST_VALUE()`.
  #
  # strategy:
  # for each column to fill:
  # 1. generate a helper column `....dbplyr_partion_x`. It creates one partition
  #    per non-NA value and all following NA (in the order of `order_by_cols`),
  #    i.e. each partition has exactly one non-NA value and any number of NA.
  # 2. use the non-NA value in each partition (`max()` is just the simplest
  #    way to do that reliable among databases).
  # 3. remove the helper column again.
  partition_sql <- purrr::map(
    cols_to_fill,
    ~ translate_sql(
      cumsum(case_when(is.na(!!.x) ~ 0L, TRUE ~ 1L)),
      con = .con,
      vars_order = translate_sql(!!!order_by_cols, con = .con),
      vars_group = op_grps(.data),
    )
  ) %>%
    set_names(paste0("..dbplyr_partion_", seq_along(cols_to_fill)))

  dp <- .data %>%
    mutate(!!!partition_sql)

  fill_sql <- purrr::map2(
    cols_to_fill, names(partition_sql),
    ~ translate_sql(
      max(!!.x, na.rm = TRUE),
      con = .con,
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
dbplyr_fill0.PostgreSQL <- dbplyr_fill0.SQLiteConnection
#' @export
dbplyr_fill0.PqConnection <- dbplyr_fill0.SQLiteConnection
#' @export
dbplyr_fill0.PostgreSQLConnection <- dbplyr_fill0.SQLiteConnection

#' @export
dbplyr_fill0.HDB <- dbplyr_fill0.SQLiteConnection

#' @export
dbplyr_fill0.ACCESS <- dbplyr_fill0.SQLiteConnection
#' @export
`dbplyr_fill0.Microsoft SQL Server` <- dbplyr_fill0.SQLiteConnection

#' @export
dbplyr_fill0.MariaDBConnection <- dbplyr_fill0.SQLiteConnection
#' @export
dbplyr_fill0.MySQLConnection <- dbplyr_fill0.SQLiteConnection
#' @export
dbplyr_fill0.MySQL <- dbplyr_fill0.SQLiteConnection
